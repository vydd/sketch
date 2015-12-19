;;;; drawing.lisp

(in-package #:sketch)

;;;  ____  ____      ___        _____ _   _  ____
;;; |  _ \|  _ \    / \ \      / /_ _| \ | |/ ___|
;;; | | | | |_) |  / _ \ \ /\ / / | ||  \| | |  _
;;; | |_| |  _ <  / ___ \ V  V /  | || |\  | |_| |
;;; |____/|_| \_\/_/   \_\_/\_/  |___|_| \_|\____|
;;;
;;;  http://onrendering.blogspot.rs/2011/10/buffer-object-streaming-in-opengl.html
;;;  http://www.java-gaming.org/index.php?topic=32169.0

(kit.gl.vao:defvao sketch-vao ()
  (:interleave ()
	     (vertex :float 2)
	     (color :unsigned-byte 4 :out-type :float)))

(defun coerce-float (x)
  (coerce x 'single-float))

(defparameter *buffer-size* (expt 2 17))
(defparameter *vertex-attributes* 3)
(defparameter *bytes-per-vertex* (+ (* 4 3)))

(defparameter *drawing-mode* :gpu)

(defun start-draw ()
  (%gl:bind-buffer :array-buffer 1)
  (%gl:buffer-data :array-buffer *buffer-size* (cffi:null-pointer) :stream-draw)
  (setf (env-buffer-position *env*) 0)
  (kit.gl.vao:vao-bind (env-vao *env*)))

(defun end-draw ()
  (%gl:bind-buffer :array-buffer 0)
  (kit.gl.vao:vao-unbind))

(defun draw-shape (primitive fill-vertices stroke-vertices)
  (kit.gl.shader:uniform-matrix (env-programs *env*) :model-m 4
				(vector (env-model-matrix *env*)))
  (when (and fill-vertices (pen-fill (env-pen *env*)))
    (push-vertices fill-vertices
		   (color-vector-255 (pen-fill (env-pen *env*)))
		   primitive))
  (when (and stroke-vertices (pen-stroke (env-pen *env*)))
    (let* ((weight (or (pen-weight (env-pen *env*)) 1))
	   (mixed (mix-lists stroke-vertices
			     (grow-polygon stroke-vertices weight))))
      (push-vertices (append mixed (list (first mixed) (second mixed)))
		     (color-vector-255 (pen-stroke (env-pen *env*)))
		     :triangle-strip))))

(defun push-vertices (vertices color primitive)
  (symbol-macrolet ((position (env-buffer-position *env*)))
    (when (> (* *bytes-per-vertex* (+ position (length vertices))) *buffer-size*)
      (start-draw))
    (let ((buffer-pointer (%gl:map-buffer-range :array-buffer
  						(* position *bytes-per-vertex*)
						(* (length vertices) *bytes-per-vertex*)
  						#x22)))
      (fill-buffer buffer-pointer vertices color)
      (%gl:draw-arrays primitive position (length vertices))
      (setf position (+ position (length vertices)))
      (%gl:unmap-buffer :array-buffer))))

(defun fill-buffer (buffer-pointer vertices color)
  (loop
     for idx from 0 by *vertex-attributes*
     for (x y) in vertices
     do (setf (cffi:mem-aref buffer-pointer :float idx) (coerce-float x)
	      (cffi:mem-aref buffer-pointer :float (+ idx 1)) (coerce-float y)
	      (cffi:mem-aref buffer-pointer :uint8 (* 4 (+ idx 2))) (aref color 0)
	      (cffi:mem-aref buffer-pointer :uint8 (+ (* 4 (+ idx 2)) 1)) (aref color 1)
	      (cffi:mem-aref buffer-pointer :uint8 (+ (* 4 (+ idx 2)) 2)) (aref color 2)
	      (cffi:mem-aref buffer-pointer :uint8 (+ (* 4 (+ idx 2)) 3)) (aref color 3))))
