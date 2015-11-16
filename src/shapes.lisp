;;;; shapes.lisp

(in-package #:sketch)

;;;  ____  _   _    _    ____  _____ ____
;;; / ___|| | | |  / \  |  _ \| ____/ ___|
;;; \___ \| |_| | / _ \ | |_) |  _| \___ \
;;;  ___) |  _  |/ ___ \|  __/| |___ ___) |
;;; |____/|_| |_/_/   \_\_|   |_____|____/

;;; 2D Primitives

(kit.gl.vao:defvao 2d-vertices ()
  (:separate ()
	     (vertex :float 2)
	     (color :float 4)))

#|
((sv (static-vectors:make-static-vector
                  (length vector)
                  :element-type 'single-float
		  :initial-element ))
 (ptr (static-vectors:static-vector-pointer sv)))
|#

(defparameter *vertex-count* (expt 2 8))
(defparameter *vertex-buffer* (make-array (* 2 *vertex-count*) :element-type 'single-float))
(defparameter *color-buffer* (make-array (* 4 *vertex-count*) :element-type 'single-float))
(defparameter *vertex-head* 0)
(defparameter *color-head* 0)

(defmacro fill-array (arr start &rest vals)
  `(setf 
    ,@(loop
	 for i from 0 below (length vals)
	 for j in vals
	 append `((aref ,arr (+ ,start ,i)) (coerce ,j 'single-float)))))

(defmacro push-vertices (&rest vals)
  `(progn
     (fill-array *vertex-buffer* *vertex-head* ,@vals)
     (setf *vertex-head* (+ ,(length vals) *vertex-head*))))

(defmacro push-colors (&rest vals)
  `(progn
     (fill-array *color-buffer* *color-head* ,@vals)
     (setf *color-head* (+ ,(length vals) *color-head*))))

(defun arc () t)

(defun ellipse (a b c d &key (mode :corner)))

(defun line (x1 y1 x2 y2))

(defun point (x y))

(defun quad (x1 y1 x2 y2 x3 y3 x4 y4))

(defun rect (a b c d &key (mode :corner))0
  (push-vertices a b a (+ b d) (+ a c) (+ b d))
  (push-colors 1.0 0.0 1.0 1.0
	       1.0 1.0 0.0 1.0
	       0.0 1.0 1.0 1.0))

(defun ngon (n a b c d &key (mode :corner) (angle 0)))

(defun triangle (x1 y1 x2 y2 x3 y3))

(defun reset-buffers ()
  (setf *vertex-head* 0)
  (setf *color-head* 0))

(defun draw-buffers ()
  (when (plusp *vertex-head*)
    (let ((vao (env-vao *env*)))

      (kit.gl.vao:vao-buffer-vector vao 0 (* 4 (length *vertex-buffer*)) *vertex-buffer*)
      (kit.gl.vao:vao-buffer-vector vao 1 (* 4 (length *color-buffer*)) *color-buffer*)
      (kit.gl.vao:vao-draw vao :first 0 :count (/ *vertex-head* 2)))))

