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
    (when (not (enough-space-for-vertices-p (length vertices)))
      (start-draw))
    ;; Important to calculate the bounding box before they've been batched so that
    ;; there are no discontinuities in the texture.
    (loop with bb = (bounding-box vertices)
          for (batch-size batch last-batch-p) in (batch-vertices vertices primitive)
          do (let* ((buffer-pointer
                      (%gl:map-buffer-range :array-buffer
                                            (* position *bytes-per-vertex*)
                                            (* batch-size *bytes-per-vertex*)
                                            +access-mode+)))
               (fill-buffer buffer-pointer
                            batch
                            color
                            :num-vertices batch-size
                            :bounding-box bb)
               (%gl:unmap-buffer :array-buffer)
               (%gl:draw-arrays primitive position batch-size)
               (incf position batch-size)
               (when (not last-batch-p)
                 (start-draw))))))

(defun enough-space-for-vertices-p (num-vertices)
  (<= (* *bytes-per-vertex*
        (+ (env-buffer-position *env*)
           num-vertices))
     *buffer-size*))

(defun batch-vertices (vertices primitive)
  (let ((num-vertices (length vertices)))
    (cond
      ((enough-space-for-vertices-p num-vertices)
       (list (list num-vertices vertices t)))
      ((eq primitive :triangles)
       ;; Assuming that the draw buffer is empty whenever we resort to batching.
       (loop with max-per-batch = (let ((buff-capacity (floor *buffer-size* *bytes-per-vertex*)))
                                    ;; This is needed to ensure that the vertices
                                    ;; for each triangle are in the same batch.
                                    (- buff-capacity (mod buff-capacity 3)))
             while (> num-vertices 2)
             for n = (min max-per-batch num-vertices)
             collect (list n vertices (zerop (- num-vertices n)))
             ;; Keep the last 2 vertices for the next batch so
             ;; that there isn't a gap in the triangle strip.
             do (setf vertices (nthcdr n vertices))
             do (decf num-vertices n)))
      ((eq primitive :triangle-strip)
       (loop with max-per-batch = (floor *buffer-size* *bytes-per-vertex*)
             while (> num-vertices 2)
             for n = (min max-per-batch num-vertices)
             collect (list n vertices (zerop (- num-vertices n)))
             ;; Keep the last 2 vertices for the next batch so
             ;; that there isn't a gap in the triangle strip.
             do (setf vertices (nthcdr (- n 2) vertices))
             do (decf num-vertices (- n 2))))
      ;; Better to fail early rather than crashing with an obscure
      ;; OpenGL error.
      (t (error "Draw buffer not large enough for this shape.")))))

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

(defun normalize-and-fit-uv-to-rect (box x y)
  (multiple-value-bind (u-in v-in)
      (normalize-to-bounding-box box x y)
    (if *uv-rect*
        (destructuring-bind (u1 v1 u-range v-range) *uv-rect*
          (values (+ u1 (* u-range u-in))
                  (+ v1 (* v-range v-in))))
        (values u-in v-in))))

(defun fill-buffer (buffer-pointer vertices color &key num-vertices bounding-box)
  (loop
    with bb = (or bounding-box
                  (bounding-box (if (null num-vertices)
                                    vertices
                                    (subseq vertices 0 num-vertices))))
    for j from 0
    while (or (null num-vertices) (< j num-vertices))
    for idx from 0 by *vertex-attributes*
    for (x y) in vertices
    do (multiple-value-bind (tx ty)
           (normalize-and-fit-uv-to-rect bb x y)
         (setf (cffi:mem-aref buffer-pointer :float idx) (coerce-float x)
               (cffi:mem-aref buffer-pointer :float (+ idx 1)) (coerce-float y)
               (cffi:mem-aref buffer-pointer :float (+ idx 2)) (coerce-float tx)
               (cffi:mem-aref buffer-pointer :float (+ idx 3)) (coerce-float (* ty (env-y-axis-sgn *env*)))
               (cffi:mem-aref buffer-pointer :uint8 (* 4 (+ idx 4))) (aref color 0)
               (cffi:mem-aref buffer-pointer :uint8 (+ (* 4 (+ idx 4)) 1)) (aref color 1)
               (cffi:mem-aref buffer-pointer :uint8 (+ (* 4 (+ idx 4)) 2)) (aref color 2)
               (cffi:mem-aref buffer-pointer :uint8 (+ (* 4 (+ idx 4)) 3)) (aref color 3))))))
