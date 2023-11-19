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
                          :accessor ,(binding-accessor b))))))

(defun define-entity-channel-observers (bindings)
  (loop for b in bindings
        when (binding-channelp b)
        collect `(define-channel-observer
                   (setf (,(binding-accessor b) win)
                         (in ,(binding-channel-name b)
                             ,(binding-initform b))))))

(defun define-entity-draw-method (name bindings body)
  `(defmethod draw ((*entity* ,name) x y &key width height mode &allow-other-keys)
     (declare (ignore x y width height mode))
     (with-accessors (,@(loop for b in bindings
                              collect `(,(binding-name b) ,(binding-accessor b))))
         *entity*
       ,@body)))

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
       ,@(define-entity-channel-observers bindings)
       ,(define-entity-prepare-method entity-name bindings)
       ,(define-entity-draw-method entity-name bindings body)

       (make-instances-obsolete ',entity-name)
       (find-class ',entity-name))))
