;;;; shapes.lisp

(in-package #:sketch)

;;;  ____  _   _    _    ____  _____ ____
;;; / ___|| | | |  / \  |  _ \| ____/ ___|
;;; \___ \| |_| | / _ \ | |_) |  _| \___ \
;;;  ___) |  _  |/ ___ \|  __/| |___ ___) |
;;; |____/|_| |_/_/   \_\_|   |_____|____/

;;; TODO: Refactor all of this and make use of indexing.

;;; 2D Primitives

(kit.gl.vao:defvao 2d-vertices ()
  (:separate ()
	     (vertex :float 2)
	     (color :float 4)))


(defparameter *vertex-count* (expt 2 18))

(defparameter *vertex-buffer* nil)
(defparameter *color-buffer* nil)

(defparameter *vertex-head* 0)
(defparameter *color-head* 0)

(defmacro fill-array (arr start &rest vals)
  `(setf 
    ,@(loop
	 for i from 0 below (length vals)
	 for j in vals
	 append `((cffi:mem-aref ,arr :float (+ ,start ,i))
		  (coerce ,j 'single-float)))))

(defmacro push-vertices (&rest vals)
  `(progn
     (fill-array *vertex-buffer* *vertex-head* ,@vals)
     (setf *vertex-head* (+ ,(length vals) *vertex-head*))))

(defmacro push-colors (&rest vals)
  `(progn
     (fill-array *color-buffer* *color-head* ,@vals)
     (setf *color-head* (+ ,(length vals) *color-head*))))

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

(defun line (x1 y1 x2 y2)
  ;; TODO: Make this sane.
  (when (pen-stroke (env-pen *env*))
    (let* ((a (atan (- y2 y1) (- x2 x1)))
	   (w (/ (or (pen-weight (env-pen *env*)) 1) 2))
	   (dx (* (sin a) w))
	   (dy (* (cos a) w)))
      (push-vertices (- x1 dx) (+ y1 dy)
		     (- x2 dx) (+ y2 dy)
		     (+ x1 dx) (- y1 dy)		     
		     (+ x1 dx) (- y1 dy)		   
		     (- x2 dx) (+ y2 dy)
		     (+ x2 dx) (- y2 dy))
      (push-stroke 6))))

(defun ellipse (a b c d &key (mode :corner))
  (ngon (truncate (* 5 (sqrt (/ (+ c d) 2)))) a b c d :mode mode))

(defun point (x y)
  (when (pen-stroke (env-pen *env*))
    (push-vertices x y x y x y)
    (push-stroke 3)))

(defun rect (a b c d &key (mode :corner))
  (when (pen-fill (env-pen *env*))
    (push-vertices a b a (+ b d) (+ a c) (+ b d)
		   a b (+ a c) (+ b d) (+ a c) b)
    (push-fill 6))
  (line a b a (+ b d))
  (line a (+ b d) (+ a c) (+ b d))
  (line (+ a c) (+ b d) (+ a c) b)
  (line (+ a c) b a b))

(defun ngon (n a b c d &key (mode :corner) (angle 0))
  (dotimes (phi n)
    (when (pen-fill (env-pen *env*))
      (push-vertices
       (+ a (* c (cos (radians (+ angle (* (/ 360 n) (1+ phi)))))))
       (+ b (* d (sin (radians (+ angle (* (/ 360 n) (1+ phi)))))))
       (+ a (* c (cos (radians (+ angle (* (/ 360 n) phi))))))
       (+ b (* d (sin (radians (+ angle (* (/ 360 n) phi))))))
       a
       b)
      (push-fill 3))
    (line
     (+ a (* c (cos (radians (+ angle (* (/ 360 n) (1+ phi)))))))
     (+ b (* d (sin (radians (+ angle (* (/ 360 n) (1+ phi)))))))
     (+ a (* c (cos (radians (+ angle (* (/ 360 n) phi))))))
     (+ b (* d (sin (radians (+ angle (* (/ 360 n) phi)))))))))

(defun triangle (x1 y1 x2 y2 x3 y3)
  (when (pen-fill (env-pen *env*))
    (push-vertices x1 y1 x2 y2 x3 y3)
    (push-fill 3))
  (line x1 y1 x2 y2)
  (line x2 y2 x3 y3)
  (line x3 y3 x1 y1))

(defun reset-buffers ()  
  (setf *vertex-head* 0
	*color-head* 0)
  (gl:bind-buffer :array-buffer 1)
  (%gl:buffer-data :array-buffer (* 2 *vertex-count*) (cffi:null-pointer) :stream-draw)
  (setf *vertex-buffer* (gl:map-buffer :array-buffer :write-only))
  (gl:bind-buffer :array-buffer 2)
  (%gl:buffer-data :array-buffer (* 4 *vertex-count*) (cffi:null-pointer) :stream-draw)
  (setf *color-buffer* (gl:map-buffer :array-buffer :write-only)))

(defun draw-buffers ()
  (when (plusp *vertex-head*)
    (let ((vao (env-vao *env*)))      
      (kit.gl.vao:vao-draw vao :first 0 :count (/ *vertex-head* 2))
      (gl:bind-buffer :array-buffer 2)
      (gl:unmap-buffer :array-buffer)
      (gl:bind-buffer :array-buffer 1)
      (gl:unmap-buffer :array-buffer)
      (gl:bind-buffer :array-buffer 0)
      (setf *vertex-buffer* nil
	    *color-buffer* nil))))
