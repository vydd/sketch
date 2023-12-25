;;;; entities.lisp

(in-package #:sketch)

;;;  _____ _   _ _____ ___ _____ ___ _____ ____
;;; | ____| \ | |_   _|_ _|_   _|_ _| ____/ ___|
;;; |  _| |  \| | | |  | |  | |  | ||  _| \___ \
;;; | |___| |\  | | |  | |  | |  | || |___ ___) |
;;; |_____|_| \_| |_| |___| |_| |___|_____|____/
;;;

;;; TODO:
;;; - Better code reuse with sketch.lisp

(defparameter *entity* nil
  "The current entity.")

(defclass entity ()
  ((width :initform 100 :accessor entity-width :initarg :width)
   (height :initform 100 :accessor entity-height :initarg :height)))

(defmethod register-entity ((sketch sketch) (entity entity) box)
  (setf (gethash entity (sketch-%entities sketch))
        ;; TODO: sb-cga shouldn't be used directly from here.
        (cons (sb-cga:inverse-matrix (env-model-matrix (sketch-%env sketch))) box)))

(defmethod initialize-instance :after ((instance entity) &rest initargs &key &allow-other-keys)
  (apply #'prepare instance initargs))

(defmethod update-instance-for-redefined-class :after
    ((instance entity) added-slots discarded-slots property-list &rest initargs)
  (declare (ignore added-slots discarded-slots property-list))
  (apply #'prepare instance initargs))

(defun define-entity-defclass (name bindings)
  `(defclass ,name (entity)
     (,@(loop for b in bindings
              unless (eq 'entity (binding-prefix b))
                collect `(,(binding-name b)
                          :initarg ,(binding-initarg b)
                          :accessor ,(binding-accessor b)
                          ,@(when (binding-channelp b) '(:allocation :class)))))))

(defun define-entity-channel-observers (entity-name bindings)
  (loop for b in bindings
        when (binding-channelp b)
        collect `(define-channel-observer
                   (setf (,(binding-accessor b) (default-entity-instance ',entity-name))
                         (in ,(binding-channel-name b)
                             ,(binding-initform b))))))

(defun define-entity-draw-method (name bindings body)
  `(defmethod draw ((*entity* ,name)
                    &key (x 0) (y 0) (width (entity-width *entity*)) (height (entity-height *entity*)) mode
                    &allow-other-keys)
     (declare (ignore mode))
     (let ((from-width width)
           (from-height height))
       (with-accessors (,@(loop for b in bindings
                                collect `(,(binding-name b) ,(binding-accessor b))))
           *entity*
         (with-translate (x y)
           (with-fit (width height from-width from-height :mode :contain)
             (register-entity *sketch* *entity* (list width height))
             ,@body))))))

(defun define-entity-prepare-method (name bindings)
  `(defmethod prepare ((*entity* ,name)
                       &key ,@(loop for b in bindings
                                    collect `((,(binding-initarg b) ,(binding-name b))
                                              ,(if (binding-defaultp b)
                                                   `(,(binding-accessor b) *entity*)
                                                   (binding-initform b))))
                       &allow-other-keys)
     (setf ,@(loop for b in bindings
                   collect `(,(binding-accessor b) *entity*)
                   collect (binding-name b)))))

(defmacro defentity (entity-name binding-forms &body body)
  (let ((bindings (parse-bindings entity-name binding-forms
                                  (class-bindings (find-class 'entity)))))
    `(progn
       ,(define-entity-defclass entity-name bindings)
       (let ((saved nil))
         (defmethod default-entity-instance ((instance (eql ',entity-name)))
           (unless saved
             (setf saved (make-instance ',entity-name)))
           saved))
       ,(define-entity-prepare-method entity-name bindings)
       ,(define-entity-draw-method entity-name bindings body)

       ,@(define-entity-channel-observers entity-name bindings)

       (make-instances-obsolete ',entity-name)
       (find-class ',entity-name))))
