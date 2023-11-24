;;;; environment.lisp

(in-package #:sketch)

;;;  _____ _   ___     _____ ____   ___  _   _ __  __ _____ _   _ _____
;;; | ____| \ | \ \   / /_ _|  _ \ / _ \| \ | |  \/  | ____| \ | |_   _|
;;; |  _| |  \| |\ \ / / | || |_) | | | |  \| | |\/| |  _| |  \| | | |
;;; | |___| |\  | \ V /  | ||  _ <| |_| | |\  | |  | | |___| |\  | | |
;;; |_____|_| \_|  \_/  |___|_| \_\\___/|_| \_|_|  |_|_____|_| \_| |_|

(defstruct env
  ;; Drawing
  (pen nil)
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
  (red-screen nil))

(defparameter *env* nil)

(defun make-white-pixel-texture ()
  "Sent to shaders when no image is active."
  (let ((texture (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-image-2d :texture-2d 0 :rgba 1 1 0 :bgra :unsigned-byte #(255 255 255 255))
    texture))

(defun initialize-environment (w)
  (with-slots ((env %env) width height y-axis) w
    (setf (env-programs env) (kit.gl.shader:compile-shader-dictionary 'sketch-programs)
          (env-vao env) (make-instance 'kit.gl.vao:vao :type 'sketch-vao)
          (env-white-pixel-texture env) (make-white-pixel-texture)
          (env-white-color-vector env) #(255 255 255 255)
          (env-pen env) (make-default-pen)
          (env-font env) (make-default-font))
    (initialize-view-matrix w)
    (kit.gl.shader:use-program (env-programs env) :fill-shader)))

(defun initialize-view-matrix (w)
  (with-slots ((env %env) width height y-axis %viewport-changed) w
    (setf (env-view-matrix env) (if (eq y-axis :down)
                                    (kit.glm:ortho-matrix 0 width height 0 -1 1)
                                    (kit.glm:ortho-matrix 0 width 0 height -1 1))
          (env-y-axis-sgn env) (if (eq y-axis :down) +1 -1)
          %viewport-changed t)))

(defun initialize-gl (w)
  (with-slots ((env %env) width height) w
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
    (gl:clear-color 0.0 1.0 0.0 1.0)
    (gl:clear :color-buffer :depth-buffer)
    (gl:flush)))

(defun debug-mode-p ()
  (and (env-red-screen *env*)
       (env-debug-key-pressed *env*)))

(defun exit-debug-mode ()
  (setf (env-red-screen *env*) nil
        (env-debug-key-pressed *env*) nil))

(defmacro with-environment (env &body body)
  `(let ((*env* ,env))
     ,@body))
