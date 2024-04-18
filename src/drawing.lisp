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
      ;; Try to clear as much space in draw buffer as possible.
      (start-draw))
    (loop for (batch-size batch last-batch-p) in (batch-vertices vertices primitive)
          do (let* ((buffer-pointer
                      (%gl:map-buffer-range :array-buffer
                                            (* position *bytes-per-vertex*)
                                            (* batch-size *bytes-per-vertex*)
                                            +access-mode+)))
               (fill-buffer buffer-pointer batch color batch-size)
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
      ((member primitive '(:triangles :triangle-strip))
       ;; Assuming that the draw buffer is empty whenever we resort to batching.
       (loop with max-per-batch = (floor *buffer-size* *bytes-per-vertex*)
             while (> num-vertices (if (eq primitive :triangles) 0 2))
             for n = (min max-per-batch num-vertices)
             for num-to-skip = (if (eq primitive :triangles)
                                   n
                                   ;; Keep the last 2 vertices for the next batch so
                                   ;; that there isn't a gap in the triangle strip.
                                   (- n 2))
             collect (list n vertices (zerop (- num-vertices n)))
             do (setf vertices (nthcdr n vertices))
             do (decf num-vertices n)))
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

(defun fit-uv-to-rect (uv)
  (if *uv-rect*
      (destructuring-bind (u-in v-in) uv
        (destructuring-bind (u1 v1 u-range v-range) *uv-rect*
          (list (+ u1 (* u-range u-in))
                (+ v1 (* v-range v-in)))))
      uv))

(defun fill-buffer (buffer-pointer vertices color &optional num-vertices)
  (loop
    for j from 0
    while (or (null num-vertices) (< j num-vertices))
    for idx from 0 by *vertex-attributes*
    for (x y) in vertices
    for (tx ty) in (mapcar #'fit-uv-to-rect (normalize-to-bounding-box vertices
                                                                       num-vertices))
    do (setf (cffi:mem-aref buffer-pointer :float idx) (coerce-float x)
             (cffi:mem-aref buffer-pointer :float (+ idx 1)) (coerce-float y)
             (cffi:mem-aref buffer-pointer :float (+ idx 2)) (coerce-float tx)
             (cffi:mem-aref buffer-pointer :float (+ idx 3)) (coerce-float (* ty (env-y-axis-sgn *env*)))
             (cffi:mem-aref buffer-pointer :uint8 (* 4 (+ idx 4))) (aref color 0)
             (cffi:mem-aref buffer-pointer :uint8 (+ (* 4 (+ idx 4)) 1)) (aref color 1)
             (cffi:mem-aref buffer-pointer :uint8 (+ (* 4 (+ idx 4)) 2)) (aref color 2)
             (cffi:mem-aref buffer-pointer :uint8 (+ (* 4 (+ idx 4)) 3)) (aref color 3))))
