;;;; shaders.lisp

(in-package #:sketch)

;;;  ____  _   _    _    ____  _____ ____  ____
;;; / ___|| | | |  / \  |  _ \| ____|  _ \/ ___|
;;; \___ \| |_| | / _ \ | | | |  _| | |_) \___ \
;;;  ___) |  _  |/ ___ \| |_| | |___|  _ < ___) |
;;; |____/|_| |_/_/   \_\____/|_____|_| \_\____/

(kit.gl.shader:defdict sketch-programs ()
  (kit.gl.shader:program :fill-shader (:view-m)
			 (:vertex-shader "
#version 330

uniform mat4 view_m;

layout (location = 0) in vec2 vertex;
layout (location = 1) in vec4 color;

smooth out vec4 f_color;

void main() {
    gl_Position = view_m * vec4(vertex, 0.0, 1.0);
    f_color = color;
}
")
			 (:fragment-shader "
#version 330

smooth in vec4 f_color;
out vec4 f_out;

void main() {
    f_out = f_color;
}
")))

(kit.gl.vao:defvao 2d-vertices ()
  (:separate ()
	     (vertex :float 2)
	     (color :float 4)))

(defparameter *vertex-count* (expt 2 18))

;;; SIZE---------------------------+
;;; HEAD-------------------------+ |
;;; POINTER-------------------+  | |
;;;                           V  V V
(defparameter *buffers* #2A((nil 0 2) (nil 0 4)))

(defun reset-buffers ()
  (dotimes (i (array-dimension *buffers* 0))
    (gl:bind-buffer :array-buffer (1+ i))
    (%gl:buffer-data :array-buffer
		     (* (aref *buffers* i 2) *vertex-count*)
		     (cffi:null-pointer)
		     :stream-draw)
    (setf (aref *buffers* i 0) (gl:map-buffer :array-buffer :write-only)
	  (aref *buffers* i 1) 0)))

(defun draw-buffers ()
  (let ((vao (env-vao *env*)))      
    (kit.gl.vao:vao-draw vao :first 0 :count (/ (aref *buffers* 0 1) 2))
    (dotimes (i (array-dimension *buffers* 0))
      (gl:bind-buffer :array-buffer (- (array-dimension *buffers* 0) i))
      (gl:unmap-buffer :array-buffer)
      (setf (aref *buffers* i 0) nil))
    (gl:bind-buffer :array-buffer 0)))

(defmacro fill-buffer (idx &rest vals)
  `(progn
     (symbol-macrolet ((buffer (aref *buffers* ,idx 0))
		       (head (aref *buffers* ,idx 1)))
       (setf 
	,@(loop
	     for i from 0 below (length vals)
	     for j in vals
	     append `((cffi:mem-aref buffer :float (+ head ,i))
		      (coerce ,j 'single-float))))
       (setf head (+ ,(length vals) head)))))

(defmacro push-vertices (&rest vals)
  `(fill-buffer 0 ,@vals))

(defmacro push-colors (&rest vals)
  `(fill-buffer 1 ,@vals))

(defmacro push-color-struct (cs)
  `(push-colors
    (color-red ,cs) (color-green ,cs) (color-blue ,cs) (color-alpha ,cs)))

(defmacro push-fill (vertices)
  `(progn
     ,@(loop for i from 0 below vertices collect
	    `(push-color-struct (pen-fill (env-pen *env*))))))

(defmacro push-stroke (vertices)
  `(progn
     ,@(loop for i from 0 below vertices collect
	    `(push-color-struct (pen-stroke (env-pen *env*))))))
