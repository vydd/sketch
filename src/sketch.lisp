;;;; sketch.lisp

(in-package #:sketch)

;;; "sketch" goes here. Hacks and glory await!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;;     _|_|_|  _|    _|  _|_|_|_|  _|_|_|_|_|    _|_|_|  _|    _|   ;;;
;;;   _|        _|  _|    _|            _|      _|        _|    _|   ;;;
;;;     _|_|    _|_|      _|_|_|        _|      _|        _|_|_|_|   ;;;
;;;         _|  _|  _|    _|            _|      _|        _|    _|   ;;;
;;;   _|_|_|    _|    _|  _|_|_|_|      _|        _|_|_|  _|    _|   ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-environment :debug-key-pressed nil)
(add-to-environment :red-screen nil)
(add-to-environment :y-axis-sgn +1)

 ;;; Sketch class

(defparameter *sketch* nil
  "The current sketch instance.")

(defparameter *default-width* 400
  "The default sketch width.")
(defparameter *default-height* 400
  "The default sketch height.")

(defclass sketch ()
  ((%env :initform nil :reader sketch-%env)
   (%setup-called :initform nil :accessor sketch-%setup-called)
   (%viewport-changed :initform t)
   (%entities :initform (make-hash-table) :accessor sketch-%entities)
   (%delayed-init-funs :initform (make-array 0 :adjustable t :fill-pointer t)
                       :accessor sketch-%delayed-init-funs)
   (title :initform "Sketch" :accessor sketch-title :initarg :title)
   (width :initform *default-width* :accessor sketch-width :initarg :width)
   (height :initform *default-height* :accessor sketch-height :initarg :height)
   (fullscreen :initform nil :accessor sketch-fullscreen :initarg :fullscreen)
   (resizable :initform nil :accessor sketch-resizable :initarg :resizable)
   (copy-pixels :initform nil :accessor sketch-copy-pixels :initarg :copy-pixels)
   (y-axis :initform :down :accessor sketch-y-axis :initarg :y-axis)
   (close-on :initform :escape :accessor sketch-close-on :initarg :close-on)))

;;; Generic functions

(defgeneric prepare (instance &key &allow-other-keys)
  (:documentation "Generated by DEFSKETCH."))

(defgeneric setup (instance &key &allow-other-keys)
  (:documentation "Called before creating the sketch.")
  (:method ((instance sketch) &key &allow-other-keys) ()))

(defgeneric draw (instance &key x y width height mode &allow-other-keys)
  (:documentation "Draws the instance with set position, dimensions, and scaling mode.")
  (:method ((instance sketch) &key x y width height mode &allow-other-keys)
    "Called repeatedly after creating the sketch window, 60fps."
    (declare (ignore x y width height mode))
    ()))

;;; Initialization

(defparameter *initialized* nil)

(defun initialize-sketch ()
  (unless *initialized*
    (setf *initialized* t)
    (kit.sdl2:init)
    (sdl2-ttf:init)
    (sdl2:in-main-thread ()
      (sdl2:gl-set-attr :multisamplebuffers 1)
      (sdl2:gl-set-attr :multisamplesamples 4)

      (sdl2:gl-set-attr :context-major-version 3)
      (sdl2:gl-set-attr :context-minor-version 3)
      (sdl2:gl-set-attr :context-profile-mask 1))))

(defmethod initialize-instance :around ((instance sketch) &key &allow-other-keys)
  (initialize-sketch)
  (sdl2:in-main-thread ()
    (call-next-method))
  (kit.sdl2:start))

(defmethod update-instance-for-redefined-class :after
    ((instance sketch) added-slots discarded-slots property-list &rest initargs)
  (declare (ignore added-slots discarded-slots property-list))
  (apply #'prepare instance initargs)
  (setf (sketch-%setup-called instance) nil)
  (setf (slot-value instance '%entities) (make-hash-table)))

;;; Error handling

(defvar *%unwind-and-call-on-error-function*)
(defmacro unwind-and-call-on-error () `(funcall *%unwind-and-call-on-error-function*))

(defmethod on-error-handler ((sketch sketch) stage error)
  (declare (ignorable sketch stage))
  (when (env-debug-key-pressed *env*)
    (with-simple-restart (:red-screen "Show red screen")
      (signal error)))
  (unwind-and-call-on-error))

(defmethod on-error ((sketch sketch) stage error)
  (declare (ignorable sketch))
  (background (ecase stage
                (:setup (rgb 0.4 0.2 0.1))
                (:draw (rgb 0.7 0 0))))
  (with-font (make-error-font)
    (with-identity-matrix
      (text (format nil "Error in ~A~%---~%~a~%---~%Click for restarts." stage error) 20 20)))
  (setf (env-red-screen *env*) t))

(defmacro with-error-handling ((sketch) &body body)
  (alexandria:with-gensyms (%error %stage)
    `(let (,%error ,%stage)
       (tagbody
          (handler-bind ((error
                           (lambda (e)
                             (setf ,%error e)
                             (let ((*%unwind-and-call-on-error-function*
                                     (lambda () (go :error))))
                               (on-error-handler ,sketch
                                                 ,%stage
                                                 ,%error)))))
            (macrolet ((with-stage (stage &body body)
                         `(progn
                            (setf ,',%stage ,stage)
                            ,@body)))
              ,@body)
            (go :end))
        :error
          (on-error ,sketch ,%stage ,%error)
        :end
          (setf (env-debug-key-pressed *env*) nil)))))

;;; Rendering

(defmacro with-sketch ((sketch) &body body)
  `(with-environment (sketch-%env ,sketch)
     (with-pen (make-default-pen)
       (with-font (make-default-font)
         (with-identity-matrix
           ,@body)))))

(defmacro with-gl-draw (&body body)
  `(progn
     (start-draw)
     ,@body
     (end-draw)))

(defun maybe-change-viewport (sketch)
  (with-slots (%env %viewport-changed width height) sketch
    (when %viewport-changed
      (kit.gl.shader:uniform-matrix (env-programs %env) :view-m 4 (vector (env-view-matrix %env)))
      (gl:viewport 0 0 width height)
      (setf %viewport-changed nil))))


;;; TODO: Would be great to move it to transforms.
(defun initialize-view-matrix (sketch)
  (with-slots ((env %env) width height y-axis %viewport-changed) sketch
    (setf (env-view-matrix env) (if (eq y-axis :down)
                                    (kit.glm:ortho-matrix 0 width height 0 -1 1)
                                    (kit.glm:ortho-matrix 0 width 0 height -1 1))
          (env-y-axis-sgn env) (if (eq y-axis :down) +1 -1)
          %viewport-changed t)))

;;; Default events

(defconstant +scancode-prefix-length+ (length "scancode-"))

(defun without-sdl2-scancode-prefix (keysym)
  (intern (subseq (symbol-name (sdl2:scancode keysym))
                  +scancode-prefix-length+)
          (find-package "KEYWORD")))

(defmethod kit.sdl2:keyboard-event :before ((instance sketch) state timestamp repeatp keysym)
  (declare (ignorable timestamp repeatp))
  (alexandria:when-let (close-on (sketch-close-on instance))
    (when (and (eql state :keyup) (eq (without-sdl2-scancode-prefix keysym) close-on))
      (kit.sdl2:close-window instance))))

;;; DEFSKETCH macro

(defun define-sketch-defclass (name bindings)
  `(defclass ,name (sketch)
     (,@(loop for b in bindings
              unless (eq 'sketch (binding-prefix b))
              collect `(,(binding-name b)
                        :initarg ,(binding-initarg b)
                        :accessor ,(binding-accessor b))))))

(defun define-sketch-channel-observers (bindings)
  (loop for b in bindings
        when (binding-channelp b)
        collect `(define-channel-observer
                   ; TODO: Should this really depend on kit.sdl2?
                   (let ((win (kit.sdl2:last-window)))
                     (when win
                       (setf (,(binding-accessor b) (window-sketch win))
                             (in ,(binding-channel-name b)
                                 ,(binding-initform b))))))))

(defun define-sketch-draw-method (name bindings body)
  `(defmethod draw ((*sketch* ,name) &key x y width height mode &allow-other-keys)
     (declare (ignore x y width height mode))
     (with-accessors (,@(loop for b in bindings
                              collect `(,(binding-name b) ,(binding-accessor b))))
         *sketch*
       ,@body)))

(defun define-sketch-prepare-method (name bindings)
  `(defmethod prepare ((*sketch* ,name)
                       &key ,@(loop for b in bindings
                                    collect `((,(binding-initarg b) ,(binding-name b))
                                              ,(if (binding-defaultp b)
                                                   `(,(binding-accessor b) *sketch*)
                                                   (binding-initform b))))
                       &allow-other-keys)
     (setf ,@(loop for b in bindings
                   collect `(,(binding-accessor b) *sketch*)
                   collect (binding-name b)))))

(defmacro defsketch (sketch-name binding-forms &body body)
  (let ((bindings (parse-bindings sketch-name binding-forms
                                  (class-bindings (find-class 'sketch)))))
    `(progn
       ,(define-sketch-defclass sketch-name bindings)
       ,@(define-sketch-channel-observers bindings)
       ,(define-sketch-prepare-method sketch-name bindings)
       ,(define-sketch-draw-method sketch-name bindings body)

       (make-instances-obsolete ',sketch-name)
       (find-class ',sketch-name))))

;;; Resource-handling

(defun delay-init-p ()
  "This checks whether the OpenGL context has been created yet. If not,
we need to wait before initializing certain resources."
  (and *sketch*
       (null (sketch-window *sketch*))))

(defun add-delayed-init-fun! (f)
  "F should be a function with no arguments."
  (vector-push-extend f (sketch-%delayed-init-funs *sketch*)))
