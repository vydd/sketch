;;;; shapes.lisp

(in-package #:sketch)

;;;  ____  _   _    _    ____  _____ ____
;;; / ___|| | | |  / \  |  _ \| ____/ ___|
;;; \___ \| |_| | / _ \ | |_) |  _| \___ \
;;;  ___) |  _  |/ ___ \|  __/| |___ ___) |
;;; |____/|_| |_/_/   \_\_|   |_____|____/

(defun point (x y)
  (declare (type real x y))
  (draw-shape :points nil `((,x ,y))))

(defun rect (x y w h)
  (declare (type real x y w h))
  (when (and (plusp w) (plusp h))
    (draw-shape
     :triangle-strip
     `((,x ,(+ y h)) (,x ,y) (,(+ x w) ,(+ y h)) (,(+ x w) ,y))
     `((,x ,y) (,x ,(+ y h)) (,(+ x w) ,(+ y h)) (,(+ x w) ,y)))))

(defun ngon (n cx cy rx ry &optional (angle 0))
  (declare (type fixnum n)
	   (type real cx cy rx ry angle))
  (let* ((angle (radians angle))
	 (rx (if (zerop rx) +epsilon+ rx))
	 (theta (/ +tau+ n))
	 (tangential (tan theta))
	 (radial (cos theta))
	 (x (* (cos angle) rx))
	 (y (* (sin angle) ry))
	 (y-mul (/ ry rx))
	 (vertices '()))
    (dotimes (i n)
      (psetf vertices (cons `(,(+ x cx) ,(+ (* y-mul y) cy)) vertices)
	     x (* radial (- x (* (- y) tangential)))
	     y (* radial (- y (* x tangential)))))
    (setf vertices (nreverse vertices))
    (draw-shape :triangle-fan vertices vertices)))

(defun ellipse (cx cy rx ry)
  (declare (type real cx cy rx ry))
  (ngon (min 24 (truncate (* 5 (sqrt (/ (+ rx ry) 2))))) cx cy rx ry))

(defun polygon (&rest vertices)
  (draw-shape
   :triangle-fan
   vertices
   vertices))
