;;;; drawing.lisp

(in-package #:sketch)

;;;  ____  ____      ___        _____ _   _  ____
;;; |  _ \|  _ \    / \ \      / /_ _| \ | |/ ___|
;;; | | | | |_) |  / _ \ \ /\ / / | ||  \| | |  _
;;; | |_| |  _ <  / ___ \ V  V /  | || |\  | |_| |
;;; |____/|_| \_\/_/   \_\_/\_/  |___|_| \_|\____|

;;;  http://onrendering.blogspot.rs/2011/10/buffer-object-streaming-in-opengl.html

(kit.gl.vao:defvao sketch-vao ()
  (:separate () (vertex :float 2)))

(defparameter *stream-buffer-capacity* (expt 2 23))

(defun start-draw ()
  (%gl:bind-buffer :array-buffer 1)
  ;(%gl:buffer-data :array-buffer *stream-buffer-capacity* (cffi:null-pointer) :stream-draw)
  ;(%gl:bind-buffer :array-buffer 0)
  )

(defun end-draw ()
  ;(%gl:unmap-buffer :array-buffer)
  (%gl:bind-buffer :array-buffer 0))


(defun flatten-vertices (vertices)
  (mapcar (lambda (x) (coerce x 'single-float))
	  (apply #'append vertices)))

(defun draw-shape (primitive fill-vertices stroke-vertices)
  (kit.gl.shader:uniform-matrix (env-programs *env*) :model-m 4
				(vector (env-model-matrix *env*)))
  (when (and fill-vertices (pen-fill (env-pen *env*)))
    (draw-fill (make-array (* 2 (length fill-vertices))
			   :element-type 'single-float
			   :initial-contents (flatten-vertices fill-vertices))
	       primitive))
  (when (and stroke-vertices (pen-stroke (env-pen *env*)))
    (let* ((weight (or (pen-weight (env-pen *env*)) 1))
	   (mixed (mix-lists stroke-vertices
			     (grow-polygon stroke-vertices weight)))
	   (vertices (append mixed (list (first mixed) (second mixed)))))
      (draw-stroke (make-array (* 2 (length vertices))
			       :element-type 'single-float
			       :initial-contents (flatten-vertices vertices))))))

(defun draw-fill (buffer primitive)
  (kit.gl.shader:uniformfv (env-programs *env*)
			   :color (color-vector (pen-fill (env-pen *env*))))
  (kit.gl.vao:vao-buffer-vector (env-vao *env*) 0 (* 4 (length buffer)) buffer :stream-draw)
  (%gl:draw-arrays primitive 0 (/ (length buffer) 2)))

(defun draw-stroke (buffer)
  (kit.gl.shader:uniformfv (env-programs *env*)
			   :color (color-vector (pen-stroke (env-pen *env*))))
  (kit.gl.vao:vao-buffer-vector (env-vao *env*) 0 (* 4 (length buffer)) buffer :stream-draw)
  (%gl:draw-arrays :triangle-strip 0 (/ (length buffer) 2)))




;;   (%gl:bind-vertex-array 0)
;;   (glBindVertexArray(vertexArrays[VERTEX_ARRAY_MD2]);
;; glDrawArrays( GL_TRIANGLES,
;;               drawOffset,
;;               md2->TriangleCount()*3);)
