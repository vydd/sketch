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
  (model-matrix (sb-cga:identity-matrix))
  (view-matrix nil)
  (matrix-stack nil)
  (y-axis-sgn +1)
  ;; Buffers
  (vert-array nil)
  ;; Streams
  (vert-stream nil)
  ;; Typography
  (font nil)
  ;; Textures
  (white-pixel-texture nil)
  (white-pixel-sampler nil)
  (white-color-vector nil)
  ;; Resources
  (resources (make-hash-table))
  ;; Debugging
  (debug-key-pressed nil)
  (red-screen nil))

(defparameter *env* nil)

(defun make-white-pixel-texture ()
  "Sent to shaders when no image is active."
  (make-texture (list (list (v!ubyte 255 255 255 255)))
                :dimensions '(1 1)
                :element-type :uint8-vec4))

(defun initialize-environment (w)
  (let* ((vert-array (make-gpu-array nil :dimensions 3
                                     :element-type 'sketch-vertex))
         (vert-stream (make-buffer-stream vert-array :retain-arrays t))
         (white-tex (make-white-pixel-texture))
         (white-sampler (sample white-tex)))
    (with-slots ((env %env) width height y-axis) w
      (setf (env-view-matrix env) (if (eq y-axis :down)
                                      (kit.glm:ortho-matrix 0 width height 0 -1 1)
                                      (kit.glm:ortho-matrix 0 width 0 height -1 1))
            (env-y-axis-sgn env) (if (eq y-axis :down) +1 -1)
            (env-vert-array env) vert-array
            (env-vert-stream env) vert-stream
            (env-white-pixel-texture env) white-tex
            (env-white-pixel-sampler env) white-sampler
            (env-white-color-vector env) #(255 255 255 255)
            (env-pen env) (make-default-pen)
            (env-font env) (make-default-font)))))

(defun initialize-gl (w)
  (with-slots ((env %env) width height) w
    (sdl2:gl-set-swap-interval 1)
    (setf (kit.sdl2:idle-render w) t)
    (gl:viewport 0 0 width height)
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
