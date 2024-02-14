;;;; window.lisp

(in-package #:sketch)

;;; __        _____ _   _ ____   _____        __
;;; \ \      / /_ _| \ | |  _ \ / _ \ \      / /
;;;  \ \ /\ / / | ||  \| | | | | | | \ \ /\ / /
;;;   \ V  V /  | || |\  | |_| | |_| |\ V  V /
;;;    \_/\_/  |___|_| \_|____/ \___/  \_/\_/

(defclass window (kit.sdl2:gl-window)
  ((sketch :initarg :sketch :accessor window-sketch
           :documentation "The sketch associated with this window.")
   (closing :initform nil :accessor window-closing)))

;; Make sure that the rendering is always enabled.

(defmethod kit.sdl2:render-enabled ((window window))
  t)

(defmethod (setf kit.sdl2:render-enabled) (value (window window))
  value)

;;; Sketch window.

(defparameter *sketch-window* (make-hash-table))

(defun sketch-window (sketch)
  (gethash sketch *sketch-window*))

;;; Backwards compatibility (sketch -> window)

(defmethod kit.sdl2:idle-render ((instance sketch))
  (kit.sdl2:idle-render (sketch-window instance)))

(defmethod (setf kit.sdl2:idle-render) (value (instance sketch))
  (setf (kit.sdl2:idle-render (sketch-window instance)) value))

(defmethod kit.sdl2:sdl-window ((instance sketch))
  (kit.sdl2:sdl-window (sketch-window instance)))

(defmethod kit.sdl2:gl-context ((instance sketch))
  (kit.sdl2:gl-context (sketch-window instance)))

(defmethod kit.sdl2:render-enabled ((instance sketch))
  (kit.sdl2:render-enabled (sketch-window instance)))

(defmethod (setf kit.sdl2:render-enabled) (value (instance sketch))
  (setf (kit.sdl2:render-enabled (sketch-window instance)) value))

;;; Non trivial sketch writers

(defmacro define-sketch-writer (slot &body body)
  `(defmethod (setf ,(alexandria:symbolicate 'sketch- slot)) :after (value (instance sketch))
     (alexandria:when-let (win (sketch-window instance))
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

;;; Backwards compatible initialization

(defmethod initialize-instance :after ((instance sketch) &rest initargs &key &allow-other-keys)
  (apply #'prepare instance initargs)
  (let ((window
          (make-instance 'window
                         :title (sketch-title instance)
                         :w (sketch-width instance)
                         :h (sketch-height instance)
                         :fullscreen (sketch-fullscreen instance)
                         :resizable (sketch-resizable instance)
                         :sketch instance)))
    (setf (gethash instance *sketch-window*) window)
    (setf (slot-value instance '%env) (make-env))
    (initialize-view-matrix instance)
    (initialize-gl window)
    ;; These will have been added in the call to PREPARE.
    (with-slots ((fs %delayed-init-funs)) instance
      (loop for f across fs
            do (funcall f))
      (setf fs (make-array 0 :adjustable t :fill-pointer t)))))

(defun initialize-gl (window)
  (handler-case (sdl2:gl-set-swap-interval 1)
    ;; Some OpenGL drivers do not allow to control swapping.
    ;; In this case SDL2 sets an error that needs to be cleared.
    (sdl2::sdl-rc-error (e)
      (warn "VSYNC was not enabled; frame rate was not restricted to 60fps.~%  ~A" e)
      (sdl2-ffi.functions:sdl-clear-error)))
  (setf (kit.sdl2:idle-render window) t)
  (gl:enable :blend :line-smooth :polygon-smooth)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:hint :line-smooth-hint :nicest)
  (gl:hint :polygon-smooth-hint :nicest)
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer :depth-buffer)
  (gl:flush))

;;; Support for resizable windows

(defmethod kit.sdl2:window-event :before ((instance window) (type (eql :size-changed)) timestamp data1 data2)
  (let ((sketch (window-sketch instance)))
    (with-slots ((env %env) width height y-axis) sketch
      (setf width data1
            height data2)
      (initialize-view-matrix sketch)))
  (kit.sdl2:render instance))

;;; Rendering

(defmethod kit.sdl2:render ((window window) &aux (sketch (window-sketch window)))
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
  (kit.sdl2:render (sketch-window instance)))

;;; Control flow

(defun stop-loop ()
  (setf (sdl2.kit:idle-render (sketch-window *sketch*)) nil))

(defun start-loop ()
  (setf (sdl2.kit:idle-render (sketch-window *sketch*)) t))

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
         `((defmethod ,name ((w window) ,@args)
             (,name (window-sketch w) ,@args)
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

;;; Close window

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
  (setf (window-closing (sketch-window instance)) t)
  (kit.sdl2:close-window (sketch-window instance)))

(defmethod kit.sdl2:close-window :around ((window window))
  (if (window-closing window)
      (call-next-method)
      (kit.sdl2:close-window (window-sketch window))))

(defmethod close-window :before ((instance window))
  (with-environment (slot-value (window-sketch instance) '%env)
    (loop for resource being the hash-values of (env-resources *env*)
          do (free-resource resource))))

(defmethod close-window :after ((instance sketch))
  (when (and *build* (not (kit.sdl2:all-windows)))
    (sdl2-ttf:quit)
    (kit.sdl2:quit)))
