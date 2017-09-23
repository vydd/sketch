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
  ;; ;; Buffer-Backed Arrays
  ;; (vert-array nil)
  ;; ;; Streams
  ;; (vert-stream nil)
  ;; Streamers
  (vert-streamer nil)
  ;; Scratch C-Array
  (vert-scratch-array nil)
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
  (let* ((vert-streamer (make-buffer-streamer 10000 'sketch-vertex))
         (scratch-arr (make-c-array nil :dimensions 1000
                                    :element-type 'sketch-vertex))
         (white-tex (make-white-pixel-texture))
         (white-sampler (sample white-tex)))
    (with-slots ((env %env) width height y-axis) w
      (setf (viewport-dimensions (current-viewport)) (list width height))
      (setf (env-view-matrix env) (if (eq y-axis :down)
                                      (kit.glm:ortho-matrix 0 width height 0 -1 1)
                                      (kit.glm:ortho-matrix 0 width 0 height -1 1))
            (env-y-axis-sgn env) (if (eq y-axis :down) +1 -1)
            (env-vert-streamer env) vert-streamer
            (env-vert-scratch-array env) scratch-arr
            (env-white-pixel-texture env) white-tex
            (env-white-pixel-sampler env) white-sampler
            (env-white-color-vector env) #(255 255 255 255)
            (env-pen env) (make-default-pen)
            (env-font env) (make-default-font)))))



(defun debug-mode-p ()
  (and (env-red-screen *env*)
       (env-debug-key-pressed *env*)))

(defun exit-debug-mode ()
  (setf (env-red-screen *env*) nil
        (env-debug-key-pressed *env*) nil))

(defmacro with-environment (env &body body)
  `(let ((*env* ,env))
     ,@body))
