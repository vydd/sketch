;;;; environment.lisp

(in-package #:sketch)

;;;  _____ _   ___     _____ ____   ___  _   _ __  __ _____ _   _ _____
;;; | ____| \ | \ \   / /_ _|  _ \ / _ \| \ | |  \/  | ____| \ | |_   _|
;;; |  _| |  \| |\ \ / / | || |_) | | | |  \| | |\/| |  _| |  \| | | |
;;; | |___| |\  | \ V /  | ||  _ <| |_| | |\  | |  | | |___| |\  | | |
;;; |_____|_| \_|  \_/  |___|_| \_\\___/|_| \_|_|  |_|_____|_| \_| |_|

;;; Temporary, until done automatically by sdl2kit
(kit.sdl2:start)
(sdl2:in-main-thread ()
  (sdl2:gl-set-attr :context-major-version 4)
  (sdl2:gl-set-attr :context-minor-version 1)
  (sdl2:gl-set-attr :context-profile-mask 1)
  
  (sdl2:gl-set-attr :red-size 8)
  (sdl2:gl-set-attr :green-size 8)
  (sdl2:gl-set-attr :blue-size 8)
  (sdl2:gl-set-attr :alpha-size 8)

  (sdl2:gl-set-attr :multisamplebuffers 1)
  (sdl2:gl-set-attr :multisamplesamples 4))
;;;

(defstruct env
  ;; Drawing
  (pen nil)
  ;; Shaders
  (programs nil)
  (view-matrix nil)
  (vao nil)
  ;; Debugging  
  (debug-key-pressed nil)
  (red-screen nil))

(defparameter *env* nil)

(defun initialize-environment (w)
  (with-slots (env width height) w
    (setf (env-programs env) (kit.gl.shader:compile-shader-dictionary
			      'sketch-programs)
	  (env-view-matrix env) (kit.glm:ortho-matrix 0 width height 0 -1 1)
	  (env-vao env) (make-instance 'kit.gl.vao:vao
				       :type '2d-vertices
				       :primitive :triangles
				       :vertex-count 0))
    (kit.gl.shader:use-program (env-programs env) :fill-shader)
    (kit.gl.shader:uniform-matrix
     (env-programs env) :view-m 4 (vector (env-view-matrix env)))))

(defun initialize-gl (w)
  (with-slots (env width height copy-pixels) w
    (sdl2:gl-set-swap-interval 1)
    (setf (kit.sdl2:idle-render w) t)
    (gl:viewport 0 0 width height)
    (gl:enable :line-smooth)
    (gl:hint :line-smooth-hint :nicest)    
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:clear-color 0.0 1.0 0.0 1.0)
    (gl:clear :color-buffer-bit)
    (gl:clear :depth-buffer-bit)
    (gl:flush)))

(defun debug-mode-p ()
  (and (env-red-screen *env*)
       (env-debug-key-pressed *env*)))

(defun exit-debug-mode ()
  (setf (env-red-screen *env*) nil
	(env-debug-key-pressed *env*) nil))

(defmacro with-environment (env &body body)
  (alexandria:once-only (env)
    `(alexandria:with-gensyms (previous-env)
       (progn
	 (setf previous-env *env*
	       *env* ,env)
	 ,@body
	 (setf *env* previous-env)))))
