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

;;; Sketch class

(defparameter *sketch* nil
  "The current sketch instance.")

(defparameter *default-width* 400
  "The default width of sketch window")
(defparameter *default-height* 400
  "The default height of sketch window")

(defparameter *restart-frames* 2)

(defclass sketch ()
  ((%env :initform (make-env) :reader sketch-%env)
   (%restart :initform t)
   (%restart-frames :initform 0)
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
   (y-axis :initform :down :accessor sketch-y-axis :initarg :y-axis)))

(defclass sketch-window (kit.sdl2:gl-window)
  ((%sketch
    :initarg :sketch
    :accessor %sketch
    :documentation "The sketch associated with this window.")
   (%closing :initform nil :accessor window-%closing)))

 ;;; Non trivial sketch writers

(defmacro define-sketch-writer (slot &body body)
  `(defmethod (setf ,(alexandria:symbolicate 'sketch- slot)) :after (value (instance sketch))
     (when (sketch-%window instance)
       (let ((win (kit.sdl2:sdl-window (sketch-%window instance))))
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
  (initialize-environment instance)
  (initialize-gl instance)
  ;; These will have been added in the call to PREPARE.
  (with-slots ((fs %delayed-init-funs)) instance
    (loop for f across fs
          do (funcall f))
    ;; Make sure there's no garbage functions hanging around.
    (loop while (not (zerop (length fs)))
          do (vector-pop fs)
          do (setf (aref fs (fill-pointer fs)) nil))))

(defmethod update-instance-for-redefined-class :after
    ((instance sketch) added-slots discarded-slots property-list &rest initargs)
  (declare (ignore added-slots discarded-slots property-list))
  (apply #'prepare instance initargs)
  (setf (slot-value instance '%restart) t
        (slot-value instance '%restart-frames) 0)
  (setf (slot-value instance '%entities) (make-hash-table)))

;;; Rendering

(defmacro gl-catch (error-color &body body)
  `(handler-case
       (progn
         ,@body)
     (error (e)
       (progn
         (background ,error-color)
         (with-font (make-error-font)
           (with-identity-matrix
             (text (format nil "ERROR~%---~%~a~%---~%Click for restarts." e) 20 20)))
         (setf %restart t
               %restart-frames *restart-frames*
               (env-red-screen *env*) t)))))

(defun draw-sketch (sketch)
  (start-draw)
  (draw sketch)
  (end-draw))

(defmacro with-sketch ((sketch) &body body)
  `(with-environment (sketch-%env ,sketch)
     (with-pen (make-default-pen)
       (with-font (make-default-font)
         (with-identity-matrix
           ,@body)))))

(defmethod kit.sdl2:render :around ((win sketch-window) &aux (instance (%sketch win)))
  (with-slots (%restart %restart-frames) instance
    ;; When %RESTART is T and %RESTART-FRAMES is positive, don't start
    ;; drawing the sketch at all.  An :AROUND method is needed to
    ;; prevent kit.sdl2's call to SDL2:GL-SWAP-WINDOW.
    (if (and %restart (plusp %restart-frames))
        (decf %restart-frames)
        (call-next-method))))

(defmethod kit.sdl2:render ((win sketch-window) &aux (instance (%sketch win)))
  (with-slots (%env %restart %restart-frames width height copy-pixels %viewport-changed) instance
    (when %viewport-changed
      (kit.gl.shader:uniform-matrix
       (env-programs %env) :view-m 4 (vector (env-view-matrix %env)))
      (gl:viewport 0 0 width height)
      (setf %viewport-changed nil))
    (with-sketch (instance)
      (unless copy-pixels
        (background (gray 0.4)))
      ;; Restart sketch on setup and when recovering from an error.
      (when %restart
        (setf %restart nil)
        (gl-catch (rgb 1 1 0.3)
          (start-draw)
          (setup instance)
          (end-draw)))
      ;; If we're in the debug mode, we exit from it immediately,
      ;; so that the restarts are shown only once. Afterwards, we
      ;; continue presenting the user with the red screen, waiting for
      ;; the error to be fixed, or for the debug key to be pressed again.
      (if (debug-mode-p)
          (progn
            (exit-debug-mode)
            (draw-sketch instance))
          (gl-catch (rgb 0.7 0 0)
            (draw-sketch instance))))))

(defmethod kit.sdl2:render ((instance sketch))
  (kit.sdl2:render (sketch-%window instance)))

;;; Support for resizable windows

(defmethod kit.sdl2:window-event :before ((instance sketch-window) (type (eql :size-changed)) timestamp data1 data2)
  (with-slots ((sketch %sketch)) instance
    (with-slots ((env %env) width height y-axis) sketch
      (setf width data1
            height data2)
      (initialize-view-matrix sketch)))
  (kit.sdl2:render instance))

;;; Default events

(defmethod kit.sdl2:keyboard-event :before ((instance sketch-window) state timestamp repeatp keysym)
  (declare (ignorable timestamp repeatp))
  (when (and (eql state :keyup)
             (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape))
    (kit.sdl2:close-window instance)))

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
