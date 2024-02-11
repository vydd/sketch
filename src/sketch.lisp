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
  "The default width of sketch window")
(defparameter *default-height* 400
  "The default height of sketch window")

(defclass sketch ()
  ((%env :initform nil :reader sketch-%env)
   (%setup-called :initform nil :accessor sketch-%setup-called)
   (%viewport-changed :initform t)
   (%entities :initform (make-hash-table) :accessor sketch-%entities)
   (%window :initform nil :accessor sketch-%window :initarg :window)
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

(defclass sketch-window (kit.sdl2:gl-window)
  ((%sketch
    :initarg :sketch
    :accessor %sketch
    :documentation "The sketch associated with this window.")
   (%closing :initform nil :accessor window-%closing)))

;; Always enabled
(defmethod kit.sdl2:render-enabled ((window sketch-window))
  t)

;; So don't do anything on SETF as well
(defmethod (setf kit.sdl2:render-enabled) (value (window sketch-window))
  value)

 ;;; Non trivial sketch writers

(defmacro define-sketch-writer (slot &body body)
  `(defmethod (setf ,(alexandria:symbolicate 'sketch- slot)) :after (value (instance sketch))
     (alexandria:when-let (win (sketch-%window instance))
       (let ((win (kit.sdl2:sdl-window win)))
         ,@body))))

(define-sketch-writer title
  (sdl2:set-window-title win value))

(define-sketch-writer width
  (sdl2:set-window-size win value (sketch-height instance))
  (initialize-view-matrix instance))

(define-sketch-writer height
  (sdl2:set-window-size win (sketch-width instance) value)
  (initialize-view-matrix instance))

(define-sketch-writer fullscreen
  (sdl2:set-window-fullscreen win value))

(define-sketch-writer resizable
  (sdl2-ffi.functions:sdl-set-window-resizable
   win
   (if value sdl2-ffi:+true+ sdl2-ffi:+false+)))

(define-sketch-writer y-axis
  (declare (ignorable win))
  (initialize-view-matrix instance))

;;; Generic functions

(defgeneric prepare (instance &key &allow-other-keys)
  (:documentation "Generated by DEFSKETCH."))

(defgeneric setup (instance &key &allow-other-keys)
  (:documentation "Called before creating the sketch window.")
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

(defmethod initialize-instance :after ((instance sketch) &rest initargs &key &allow-other-keys)
  (apply #'prepare instance initargs)
  (setf (sketch-%window instance)
        (make-instance 'sketch-window
                       :title (sketch-title instance)
                       :w (sketch-width instance)
                       :h (sketch-height instance)
                       :fullscreen (sketch-fullscreen instance)
                       :resizable (sketch-resizable instance)
                       :sketch instance))
  (setf (slot-value instance '%env) (make-env))
  (initialize-view-matrix instance)
  (initialize-gl instance)
  ;; These will have been added in the call to PREPARE.
  (with-slots ((fs %delayed-init-funs)) instance
    (loop for f across fs
          do (funcall f))
    (setf fs (make-array 0 :adjustable t :fill-pointer t))))

(defun initialize-gl (sketch)
  (with-slots ((w %window)) sketch
    (handler-case (sdl2:gl-set-swap-interval 1)
      ;; Some OpenGL drivers do not allow to control swapping.
      ;; In this case SDL2 sets an error that needs to be cleared.
      (sdl2::sdl-rc-error (e)
        (warn "VSYNC was not enabled; frame rate was not restricted to 60fps.~%  ~A" e)
        (sdl2-ffi.functions:sdl-clear-error)))
    (setf (kit.sdl2:idle-render w) t)
    (gl:enable :blend :line-smooth :polygon-smooth)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:hint :line-smooth-hint :nicest)
    (gl:hint :polygon-smooth-hint :nicest)
    (gl:clear-color 0.0 0.0 0.0 1.0)
    (gl:clear :color-buffer :depth-buffer)
    (gl:flush)))

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

(defmethod kit.sdl2:render ((win sketch-window) &aux (sketch (%sketch win)))
  (maybe-change-viewport sketch)
  (with-sketch (sketch)
    (with-gl-draw
      (with-error-handling (sketch)
        (unless (sketch-copy-pixels sketch)
          (background (gray 0.4)))
        (when (or (env-red-screen *env*)
                  (not (sketch-%setup-called sketch)))
          (setf (env-red-screen *env*) nil
                (sketch-%setup-called sketch) t)
          (with-stage :setup
            (setup sketch)))
        (with-stage :draw
          (draw sketch))))))

(defmethod kit.sdl2:render ((instance sketch))
  (kit.sdl2:render (sketch-%window instance)))

;;; TODO: Would be great to move it to transforms.
(defun initialize-view-matrix (sketch)
  (with-slots ((env %env) width height y-axis %viewport-changed) sketch
    (setf (env-view-matrix env) (if (eq y-axis :down)
                                    (kit.glm:ortho-matrix 0 width height 0 -1 1)
                                    (kit.glm:ortho-matrix 0 width 0 height -1 1))
          (env-y-axis-sgn env) (if (eq y-axis :down) +1 -1)
          %viewport-changed t)))

;;; Support for resizable windows

(defmethod kit.sdl2:window-event :before ((instance sketch-window) (type (eql :size-changed)) timestamp data1 data2)
  (with-slots ((sketch %sketch)) instance
    (with-slots ((env %env) width height y-axis) sketch
      (setf width data1
            height data2)
      (initialize-view-matrix sketch)))
  (kit.sdl2:render instance))

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

(defmethod close-window :before ((instance sketch-window))
  (with-environment (slot-value (%sketch instance) '%env)
    (loop for resource being the hash-values of (env-resources *env*)
       do (free-resource resource))))

(defmethod close-window :after ((instance sketch))
  (when (and *build* (not (kit.sdl2:all-windows)))
    (sdl2-ttf:quit)
    (kit.sdl2:quit)))

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
                       (setf (,(binding-accessor b) (%sketch win))
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

;;; Control flow

(defun stop-loop ()
  (setf (sdl2.kit:idle-render (sketch-%window *sketch*)) nil))

(defun start-loop ()
  (setf (sdl2.kit:idle-render (sketch-%window *sketch*)) t))

;;; Backward compatibility.
;; Previously, the main `sketch` class inherited from
;; `kit.sdl2:gl-window`, and input was handled by specialising on methods from
;; sdl2kit. So we need to forward sdl2kit input calls to the `sketch` class for
;; old sketches that rely on that approach.
(defmacro define-sdl2-forward (name (&rest args) &optional already-defined?)
  `(progn
     ;; An empty method so we don't get an error if we try to forward
     ;; when the user hasn't defined it.
     (defmethod ,name ((w sketch) ,@args))
     ,@(when (not already-defined?)
         `((defmethod ,name ((w sketch-window) ,@args)
             (,name (%sketch w) ,@args)
             (call-next-method))))))
(define-sdl2-forward kit.sdl2:mousebutton-event (state timestamp button x y) t)
(define-sdl2-forward kit.sdl2:mousemotion-event (timestamp button-mask x y xrel yrel) t)
(define-sdl2-forward kit.sdl2:textinput-event (timestamp text))
(define-sdl2-forward kit.sdl2:keyboard-event (state timestamp repeatp keysym))
(define-sdl2-forward kit.sdl2:mousewheel-event (timestamp x y))
(define-sdl2-forward kit.sdl2:window-event (type timestamp data1 data2))
(define-sdl2-forward kit.sdl2:controller-added-event (c))
(define-sdl2-forward kit.sdl2:controller-removed-event (c))
(define-sdl2-forward kit.sdl2:controller-axis-motion-event (controller timestamp axis value))
(define-sdl2-forward kit.sdl2:controller-button-event (controller state timestamp button))

(defmethod kit.sdl2:idle-render ((instance sketch))
  (kit.sdl2:idle-render (sketch-%window instance)))

(defmethod (setf kit.sdl2:idle-render) (value (instance sketch))
  (setf (kit.sdl2:idle-render (sketch-%window instance)) value))

(defmethod kit.sdl2:sdl-window ((instance sketch))
  (kit.sdl2:sdl-window (sketch-%window instance)))

(defmethod kit.sdl2:gl-context ((instance sketch))
  (kit.sdl2:gl-context (sketch-%window instance)))

(defmethod kit.sdl2:render-enabled ((instance sketch))
  (kit.sdl2:render-enabled (sketch-%window instance)))

(defmethod (setf kit.sdl2:render-enabled) (value (instance sketch))
  (setf (kit.sdl2:render-enabled (sketch-%window instance)) value))

;; KIT.SDL2:CLOSE-WINDOW is tricky: it should always be called on both
;; the sketch and sketch's window; but it also can be first called on
;; both the window or the sketch.
;; It also should be called in sdl2's main thread, which is done by an
;; :AROUND method defined on KIT.SDL2:WINDOW.
;; The primary method defined on the SKETCH-WINDOW should
;; (call-next-method) because there is a primary method defined on
;; GL-WINDOW.
;; Finally, the :AFTER method defined on SKETCH calls KIT.SDL2:QUIT and
;; SDL2-TTF:QUIT.
(defmethod kit.sdl2:close-window ((instance sketch))
  (with-slots ((window %window)) instance
    (setf (window-%closing window) t)
    (kit.sdl2:close-window window)))

(defmethod kit.sdl2:close-window :around ((instance sketch-window))
  (if (window-%closing instance)
      (call-next-method)
      (kit.sdl2:close-window (%sketch instance))))

;;; Resource-handling

(defun delay-init-p ()
  "This checks whether the OpenGL context has been created yet. If not,
we need to wait before initializing certain resources."
  (and *sketch*
       (null (sketch-%window *sketch*))))

(defun add-delayed-init-fun! (f)
  "F should be a function with no arguments."
  (vector-push-extend f (sketch-%delayed-init-funs *sketch*)))
