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
(defparameter +access-mode+
  (cffi:foreign-bitfield-value '%gl:BufferAccessMask
                               '(:map-write :map-unsynchronized)))

(defparameter *draw-mode* :gpu)
(defparameter *draw-sequence* nil)

(defparameter *uv-rect* nil)

(defmacro with-uv-rect (rect &body body)
  `(let ((*uv-rect* ,rect))
     ,@body))

(defun start-draw ()
  (%gl:bind-buffer :array-buffer (aref (slot-value (env-vao *env*) 'kit.gl.vao::vbos) 0))
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
    (cropped-image (values (env-white-color-vector *env*)
                           (or (image-texture res) (env-white-pixel-texture *env*))
                           (cropped-image-uv-rect res)))
    (image (values (env-white-color-vector *env*)
                   (or (image-texture res) (env-white-pixel-texture *env*))))))

(defun draw-shape (primitive fill-vertices stroke-vertices)
  (when (and fill-vertices (pen-fill (env-pen *env*)))
    (multiple-value-bind (shader-color shader-texture uv-rect)
        (shader-color-texture-values (pen-fill (env-pen *env*)))
      (with-uv-rect uv-rect
        (push-vertices fill-vertices
                       shader-color
                       shader-texture
                       primitive
                       *draw-mode*))))
  (when (and stroke-vertices (pen-stroke (env-pen *env*)))
    (multiple-value-bind (shader-color shader-texture uv-rect)
        (shader-color-texture-values (pen-stroke (env-pen *env*)))
      (with-uv-rect uv-rect
        (let* ((weight (or (pen-weight (env-pen *env*)) 1))
               (mixed (mix-lists stroke-vertices
                                 (grow-polygon stroke-vertices weight))))
          (push-vertices (append mixed (list (first mixed) (second mixed)))
                         shader-color
                         shader-texture
                         :triangle-strip
                         *draw-mode*))))))

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
                                                +access-mode+)))
      (fill-buffer buffer-pointer vertices color)
      (%gl:unmap-buffer :array-buffer)
      (%gl:draw-arrays primitive position (length vertices))
      (setf position (+ position (length vertices))))))

(defmethod push-vertices (vertices color texture primitive (draw-mode (eql :figure)))
  (let* ((vertices (mapcar (lambda (v) (transform-vertex v (env-model-matrix *env*)))
                           vertices))
         (buffer (static-vectors:make-static-vector
                  (* *bytes-per-vertex* (length vertices))
                  :element-type '(unsigned-byte 8)))
         (buffer-pointer (static-vectors:static-vector-pointer buffer)))
    (fill-buffer buffer-pointer vertices color)
    (push (list :primitive primitive
                :pointer buffer-pointer
                :length (length vertices))
          *draw-sequence*)))

(defmethod push-vertices (vertices color texture primitive (draw-mode null))
  ;; TODO: Drawing in event handlers could be useful with COPY-PIXELS set to to T.
  (warn "Can't draw from current context (e.g. an event handler)."))

(defun draw-polygon (points)
  (let ((tobj (make-instance 'polygon-tessellator)))
    (glu:tess-property tobj :winding-rule :positive)
    (glu:with-tess-polygon (tobj)
      (glu:with-tess-contour tobj
        (loop for xy in points
              ;; Expects 3d coordinates.
              for coord-3d = (append xy '(0))
              do (glu:tess-vertex tobj coord-3d coord-3d))))
    (glu:tess-delete tobj))
  ;; Let DRAW-SHAPE handle drawing the stroke, if any.
  (draw-shape nil nil points))

(defclass polygon-tessellator (glu:tessellator)
  ())

(defmethod glu:vertex-data-callback ((tess polygon-tessellator) vertex-data polygon-data)
  (gl:vertex (first vertex-data) (second vertex-data) (third vertex-data)))

(defmethod glu:error-data-callback ((tess polygon-tessellator) error-num polygon-data)
  (error (format nil "Tessellation error: ~a" error-num)))

(defun fit-uv-to-rect (uv)
  (if *uv-rect*
      (destructuring-bind (u-in v-in) uv
        (destructuring-bind (u1 v1 u-range v-range) *uv-rect*
          (list (+ u1 (* u-range u-in))
                (+ v1 (* v-range v-in)))))
      uv))

(defun fill-buffer (buffer-pointer vertices color)
  (loop
    for idx from 0 by *vertex-attributes*
    for (x y) in vertices
    for (tx ty) in (mapcar #'fit-uv-to-rect (normalize-to-bounding-box vertices))
    do (setf (cffi:mem-aref buffer-pointer :float idx) (coerce-float x)
             (cffi:mem-aref buffer-pointer :float (+ idx 1)) (coerce-float y)
             (cffi:mem-aref buffer-pointer :float (+ idx 2)) (coerce-float tx)
             (cffi:mem-aref buffer-pointer :float (+ idx 3)) (coerce-float (* ty (env-y-axis-sgn *env*)))
             (cffi:mem-aref buffer-pointer :uint8 (* 4 (+ idx 4))) (aref color 0)
             (cffi:mem-aref buffer-pointer :uint8 (+ (* 4 (+ idx 4)) 1)) (aref color 1)
             (cffi:mem-aref buffer-pointer :uint8 (+ (* 4 (+ idx 4)) 2)) (aref color 2)
             (cffi:mem-aref buffer-pointer :uint8 (+ (* 4 (+ idx 4)) 3)) (aref color 3))))
