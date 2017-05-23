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

(kit.gl.vao:defvao sketch-vao ()
  (:interleave ()
               (vertex :float 2)
               (texcoord :float 2)
               (color :unsigned-byte 4 :out-type :float)))

(defparameter *buffer-size* (expt 2 17))
(defparameter *vertex-attributes* 5)
(defparameter *bytes-per-vertex* (+ (* 4 *vertex-attributes*)))

(defparameter *draw-mode* :gpu)
(defparameter *draw-sequence* nil)

(defun start-draw ()
  (%gl:bind-buffer :array-buffer 1)
  (%gl:buffer-data :array-buffer *buffer-size* (cffi:null-pointer) :stream-draw)
  (setf (env-buffer-position *env*) 0)
  (kit.gl.vao:vao-bind (env-vao *env*)))

(defun end-draw ()
  (%gl:bind-buffer :array-buffer 0)
  (kit.gl.vao:vao-unbind))

(defun shader-color-texture-values (res)
  (typecase res
    (color (values (or (color-vector-255 res) (env-white-color-vector *env*))
                   (env-white-pixel-texture *env*)))
    (image (values (env-white-color-vector *env*)
                   (or (image-texture res) (env-white-pixel-texture *env*))))))

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
  (kit.gl.shader:uniform-matrix (env-programs *env*) :model-m 4
                                (vector (env-model-matrix *env*)))
  (gl:bind-texture :texture-2d texture)
  (symbol-macrolet ((position (env-buffer-position *env*)))
    (when (> (* *bytes-per-vertex* (+ position (length vertices))) *buffer-size*)
      (start-draw))
    (let ((buffer-pointer (%gl:map-buffer-range :array-buffer
                                                (* position *bytes-per-vertex*)
                                                (* (length vertices) *bytes-per-vertex*)
                                                #x22)))
      (fill-buffer buffer-pointer vertices color)
      (%gl:unmap-buffer :array-buffer)
      (%gl:draw-arrays primitive position (length vertices))
      (setf position (+ position (length vertices))))))

(defmethod push-vertices (vertices color texture primitive (draw-mode (eql :figure)))
  (let* ((buffer (static-vectors:make-static-vector
                  (* *bytes-per-vertex* (length vertices))
                  :element-type '(unsigned-byte 8)))
         (buffer-pointer (static-vectors:static-vector-pointer buffer)))
    (fill-buffer buffer-pointer vertices color)
    (push (list :primitive primitive
                :pointer buffer-pointer
                :length (length vertices)) *draw-sequence*)))

(defun fill-buffer (buffer-pointer vertices color)
  (loop
     for idx from 0 by *vertex-attributes*
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
