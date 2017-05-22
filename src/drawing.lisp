;;;; drawing.lisp

(in-package #:sketch)

;;;  ____  ____      ___        _____ _   _  ____
;;; |  _ \|  _ \    / \ \      / /_ _| \ | |/ ___|
;;; | | | | |_) |  / _ \ \ /\ / / | ||  \| | |  _
;;; | |_| |  _ <  / ___ \ V  V /  | || |\  | |_| |
;;; |____/|_| \_\/_/   \_\_/\_/  |___|_| \_|\____|
;;;
;;;  http://onrendering.blogspot.com/2011/10/buffer-object-streaming-in-opengl.html
;;;  http://www.java-gaming.org/index.php?topic=32169.0

(defconstant +vertex-attributes+ 5)
(defconstant +bytes-per-vertex+ (+ (* 4 +vertex-attributes+)))

(defparameter *draw-mode* :gpu)
(defparameter *draw-sequence* nil)

(defun shader-color-texture-values (res)
  (typecase res
    (color (values (or (color-vector-255 res) (env-white-color-vector *env*))
                   (env-white-pixel-sampler *env*)))
    (image (values (env-white-color-vector *env*)
                   (or (image-sampler res) (env-white-pixel-sampler *env*))))))

(defun draw-shape (primitive fill-vertices stroke-vertices)
  (when (and fill-vertices (pen-fill (env-pen *env*)))
    (multiple-value-bind (shader-color shader-texture)
        (shader-color-texture-values (pen-fill (env-pen *env*)))
      (push-vertices fill-vertices
                     shader-color
                     shader-texture
                     primitive
                     *draw-mode*)))
  (when (and stroke-vertices (pen-stroke (env-pen *env*)))
    (multiple-value-bind (shader-color shader-texture)
        (shader-color-texture-values (pen-stroke (env-pen *env*)))
      (let* ((weight (or (pen-weight (env-pen *env*)) 1))
             (mixed (mix-lists stroke-vertices
                               (grow-polygon stroke-vertices weight))))
        (push-vertices (append mixed (list (first mixed) (second mixed)))
                       shader-color
                       shader-texture
                       :triangle-strip
                       *draw-mode*)))))

(defmethod push-vertices (vertices color texture primitive (draw-mode (eql :gpu)))
  (let* ((env *env*)
         (streamer (env-vert-streamer env))
         (scratch-arr (env-vert-scratch-array env))
         (len (length vertices)))
    (let ((ptr (c-array-pointer scratch-arr)))
      (fill-buffer ptr vertices color))
    (buffer-streamer-push (subseq-c scratch-arr 0 len) streamer
                          primitive)
    (fill-primitive texture)))

(defmethod push-vertices (vertices color texture primitive (draw-mode (eql :figure)))
  (let* ((buffer (static-vectors:make-static-vector
                  (* +bytes-per-vertex+ (length vertices))
                  :element-type '(unsigned-byte 8)))
         (buffer-pointer (static-vectors:static-vector-pointer buffer)))
    (fill-buffer buffer-pointer vertices color)
    (push (list :primitive primitive
                :pointer buffer-pointer
                :length (length vertices)) *draw-sequence*)))

(defun fill-buffer (buffer-pointer vertices color)
  (declare (optimize speed))
  (loop
     for idx from 0 by +vertex-attributes+
     for (x y) in vertices
     for (tx ty) in (normalize-to-bounding-box vertices)
     do (setf (cffi:mem-aref buffer-pointer :float idx) (coerce-float x)
              (cffi:mem-aref buffer-pointer :float (+ idx 1)) (coerce-float y)
              (cffi:mem-aref buffer-pointer :float (+ idx 2)) (coerce-float tx)
              (cffi:mem-aref buffer-pointer :float (+ idx 3)) (coerce-float (* ty (env-y-axis-sgn *env*)))
              (cffi:mem-aref buffer-pointer :uint8 (* 4 (+ idx 4))) (aref color 0)
              (cffi:mem-aref buffer-pointer :uint8 (+ (* 4 (+ idx 4)) 1)) (aref color 1)
              (cffi:mem-aref buffer-pointer :uint8 (+ (* 4 (+ idx 4)) 2)) (aref color 2)
              (cffi:mem-aref buffer-pointer :uint8 (+ (* 4 (+ idx 4)) 3)) (aref color 3))))
