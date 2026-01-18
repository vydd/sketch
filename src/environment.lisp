;;;; environment.lisp

(in-package #:sketch)

;;;  _____ _   ___     _____ ____   ___  _   _ __  __ _____ _   _ _____
;;; | ____| \ | \ \   / /_ _|  _ \ / _ \| \ | |  \/  | ____| \ | |_   _|
;;; |  _| |  \| |\ \ / / | || |_) | | | |  \| | |\/| |  _| |  \| | | |
;;; | |___| |\  | \ V /  | ||  _ <| |_| | |\  | |  | | |___| |\  | | |
;;; |_____|_| \_|  \_/  |___|_| \_\\___/|_| \_|_|  |_|_____|_| \_| |_|

(defstruct env
  ;; Drawing
  (programs nil)
  (model-matrix (sb-cga:identity-matrix)) ; TODO: sb-cga shouldn't be used directly from here
  (view-matrix nil)
  (matrix-stack nil)
  (y-axis-sgn +1)
  (vao nil)
  (buffer-position 0)
  ;; Typography
  (font nil)
  ;; Textures
  (white-pixel-texture nil)
  (white-color-vector nil)
  ;; Resources
  (resources (make-hash-table))
  ;; Debugging
  (debug-key-pressed nil)
  (red-screen nil)
  ;; Extensible properties
  (extensions (make-hash-table)))

(defparameter *env* nil)

;;; Extensible environment properties
;;;
;;; This system allows new environment properties to be defined outside of this
;;; file, near the code that uses them. Use DEFINE-ENVIRONMENT-PROPERTY to add
;;; new properties with automatic accessor generation and initialization.

(defparameter *environment-initializers* (make-hash-table)
  "Registry of initializer functions for environment properties.
Keys are property names (keywords), values are thunks that return initial values.")

(defun register-environment-initializer (name initializer)
  "Register an initializer function for environment property NAME."
  (setf (gethash name *environment-initializers*) initializer))

(defun get-environment-extension (env name)
  "Get an extension property from ENV."
  (gethash name (env-extensions env)))

(defun set-environment-extension (env name value)
  "Set an extension property in ENV."
  (setf (gethash name (env-extensions env)) value))

(defun initialize-environment-extensions (env)
  "Initialize all registered extension properties in ENV."
  (maphash (lambda (name initializer)
             (set-environment-extension env name (funcall initializer)))
           *environment-initializers*))

(defmacro define-environment-property (name &body initializer)
  "Define an extensible environment property.

NAME should be a keyword like :my-property.
INITIALIZER is code that returns the initial value (evaluated at env creation time).

This generates:
  - (env-NAME env) accessor function
  - (setf (env-NAME env) value) setter
  - Registration of the initializer

Example:
  (define-environment-property :my-cache
    (make-hash-table))

Then use: (env-my-cache *env*)"
  (let* ((name-string (if (keywordp name)
                          (symbol-name name)
                          (string name)))
         (accessor (alexandria:symbolicate 'env- name-string))
         (setter (alexandria:symbolicate 'set-env- name-string)))
    `(progn
       (register-environment-initializer ,name (lambda () ,@initializer))
       (defun ,accessor (env)
         (get-environment-extension env ,name))
       (defun ,setter (env value)
         (set-environment-extension env ,name value))
       (defsetf ,accessor ,setter)
       ,name)))

(defun make-white-pixel-texture ()
  "Sent to shaders when no image is active."
  (let ((texture (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-image-2d :texture-2d 0 :rgba 1 1 0 :bgra :unsigned-byte #(255 255 255 255))
    texture))

(defun initialize-environment (sketch)
  (with-slots ((env %env) width height y-axis) sketch
    (setf (env-programs env) (kit.gl.shader:compile-shader-dictionary 'sketch-programs)
          (env-vao env) (make-instance 'kit.gl.vao:vao :type 'sketch-vao)
          (env-white-pixel-texture env) (make-white-pixel-texture)
          (env-white-color-vector env) #(255 255 255 255)
          (env-font env) (make-default-font))
    (initialize-view-matrix sketch)
    (initialize-environment-extensions env)
    (kit.gl.shader:use-program (env-programs env) :fill-shader)))

(defun initialize-view-matrix (sketch)
  (with-slots ((env %env) width height y-axis %viewport-changed) sketch
    (setf (env-view-matrix env) (if (eq y-axis :down)
                                    (kit.glm:ortho-matrix 0 width height 0 -1 1)
                                    (kit.glm:ortho-matrix 0 width 0 height -1 1))
          (env-y-axis-sgn env) (if (eq y-axis :down) +1 -1)
          %viewport-changed t)))

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

(defmacro with-environment (env &body body)
  `(let ((*env* ,env))
     ,@body))
