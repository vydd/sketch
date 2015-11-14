;;;; shapes.lisp

(in-package #:sketch)

;;;  ____  _   _    _    ____  _____ ____
;;; / ___|| | | |  / \  |  _ \| ____/ ___|
;;; \___ \| |_| | / _ \ | |_) |  _| \___ \
;;;  ___) |  _  |/ ___ \|  __/| |___ ___) |
;;; |____/|_| |_/_/   \_\_|   |_____|____/

;;; 2D Primitives

(defmacro with-fill-and-stroke (primitive &body body))

(defun arc () t)

(defun ellipse (a b c d &key (mode :corner)))

(defun line (x1 y1 x2 y2)
  (apply #'gl:color (color-rgba (pen-stroke (env-pen *env*))))
  (gl:line-width (alexandria:clamp
		  (pen-weight (env-pen *env*))
		  1.0 10.0))
  (gl:with-primitive :lines
    (gl:vertex x1 y1)
    (gl:vertex x2 y2)))

(defun line2 (x1 y1 x2 y2)
  (apply #'gl:color (color-rgba (pen-stroke (env-pen *env*))))
  (let* ((w (/ (pen-weight (env-pen *env*)) 2))
	 (a (atan (- y2 y1) (- x2 x1)))
	 (dx (* 9/10 (sin a) w))
	 (dy (* 9/10 (cos a) w)))
    (gl:with-primitive :triangle-strip
      (gl:vertex (- x1 dx) (+ y1 dy))
      (gl:vertex (- x2 dx) (+ y2 dy))
      (gl:vertex (+ x1 dx) (- y1 dy))
      (gl:vertex (+ x2 dx) (- y2 dy)))))

(defun point (x y))

(defun quad (x1 y1 x2 y2 x3 y3 x4 y4))

(defun rect (a b c d &key (mode :corners)))

(defun ngon (n a b c d &key (mode :corner) (angle 0)))

(defun triangle (x1 y1 x2 y2 x3 y3))



#|
(defmacro with-fill-and-stroke (primitive &body body)
  `(progn (when (env-fill *env*)
	    (apply #'gl:color (color-rgba (env-fill *env*)))
	    (gl:with-primitive ,primitive
	      ,@body))
	  (when (env-stroke *env*)
	    (apply #'gl:color (color-rgba (env-stroke *env*)))
	    (gl:line-width (alexandria:clamp
	    		    (env-stroke-weight *env*)
	    		    1.0 10.0))
	    (gl:with-primitive :line-loop
	      ,@body))))

(defun arc () t)

(defun ellipse (a b c d &key (mode :corner))
  (ngon 90 a b c d :mode mode))

(defun line (x1 y1 x2 y2)
  (when (env-stroke *env*)
    (apply #'gl:color (color-rgba (env-stroke *env*)))
    (gl:with-primitive :lines
      (gl:vertex x1 y1)
      (gl:vertex x2 y2))))

(defun point (x y)
  (apply #'gl:color (color-rgba (env-stroke *env*)))
  (gl:with-primitive :points
    (gl:vertex x y 0.0)))

(defun quad (x1 y1 x2 y2 x3 y3 x4 y4)
  (with-fill-and-stroke :quads
    (gl:vertex x1 y1)
    (gl:vertex x2 y2)
    (gl:vertex x3 y3)
    (gl:vertex x4 y4)))

(defun rect (a b c d &key (mode :corners))
  (case mode
    (:corners (quad a b c b c d a d))
    (:center (quad (- a (/ c 2)) (- b (/ d 2)) (+ a (/ c 2)) (- b (/ d 2))
		   (- a (/ c 2)) (+ b (/ d 2)) (+ a (/ c 2)) (+ b (/ d 2))))
    (:radius (quad (- a c) (- b d) (+ a c) (- b d)
		   (+ a c) (+ b d) (- a c) (+ b d)))))

(defun ngon (n a b c d &key (mode :corner) (angle 0))
  (with-fill-and-stroke :triangle-fan
    (case mode
      (:corner       
       (dotimes (phi n)
	 (gl:vertex (+ a (* c (cos (radians (+ angle (* (/ 360 n) phi))))))
		    (+ b (* d (sin (radians (+ angle (* (/ 360 n) phi))))))))))))

(defun triangle (x1 y1 x2 y2 x3 y3)
  (with-fill-and-stroke :triangles
    (gl:vertex x1 y1)
    (gl:vertex x2 y2)
    (gl:vertex x3 y3)))
|#
