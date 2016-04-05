;;;; shapes.lisp

(in-package #:sketch)

;;;  ____  _   _    _    ____  _____ ____
;;; / ___|| | | |  / \  |  _ \| ____/ ___|
;;; \___ \| |_| | / _ \ | |_) |  _| \___ \
;;;  ___) |  _  |/ ___ \|  __/| |___ ___) |
;;; |____/|_| |_/_/   \_\_|   |_____|____/

(defun point (x y)
  (declare (type real x y))
  (rect x y 1 1))

(defun line (x1 y1 x2 y2)
  (declare (type real x1 y1 x2 y2))
  (let* ((a (atan (- y2 y1) (- x2 x1)))
	 (w (/ (or (pen-weight (env-pen *env*)) 1) 2))
	 (dx (* 2 (sin a) w))
	 (dy (* 2 (cos a) w))
	 (dx+ (floor dx 2))
	 (dx- (- dx dx+))
	 (dy+ (floor dy 2))
	 (dy- (- dy dy+)))
    (with-pen (flip-pen (env-pen *env*))
      (draw-shape
       :triangle-strip
       `((,(- x1 dx-) ,(+ y1 dy+))
	 (,(- x2 dx-) ,(+ y2 dy+))
	 (,(+ x1 dx+) ,(- y1 dy-))
	 (,(+ x2 dx+) ,(- y2 dy-)))
       nil))))

(defun translated-intersects (lines distance)
  (let ((lines (mapcar (lambda (x) (translate-line x distance)) lines)))
    (edges (append (list (caar lines))
		   (mapcar (lambda (x) (apply #'intersect-lines x))
			   (edges lines nil))
		   (cdar (last lines)))
	   nil)))

(defun polyline (&rest coordinates)
  (case (pen-weight (env-pen *env*))
    (nil nil)
    (1 (mapcar (lambda (x) (line (caar x) (cadar x) (caadr x) (cadadr x)))
	       (edges (group coordinates) nil)))
    (t (multiple-value-bind (d+ d-)
	   (div2-inexact (pen-weight (env-pen *env*)))
	 (let* ((lines (edges (group coordinates) nil))
		(lefts (translated-intersects lines (+ d+)))
		(rights (translated-intersects lines (- d-))))
	   (with-pen (flip-pen (env-pen *env*))
	     (draw-shape
	      :triangle-strip
	      (mix-lists (apply #'append lefts)
			 (apply #'append rights))
	      nil))
	   ;; (with-pen (make-pen :fill (rgb 0 1 0))
	   ;;   (mapcar (lambda (x) (circle (caar x) (cadar x) 0.04)) rights))
	   )))))

(defun rect (x y w h)
  (declare (type real x y w h))
  (when (and (plusp w) (plusp h))
    (draw-shape
     :triangle-strip
     `((,x ,(+ y h)) (,x ,y) (,(+ x w) ,(+ y h)) (,(+ x w) ,y))
     `((,x ,y) (,x ,(+ y h)) (,(+ x w) ,(+ y h)) (,(+ x w) ,y)))))

(defun ngon-vertices (n cx cy rx ry &optional (angle 0))
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
    (nreverse vertices)))

(defun ngon (n cx cy rx ry &optional (angle 0))
  (declare (type fixnum n)
	   (type real cx cy rx ry angle))
  (let ((vertices (ngon-vertices n cx cy rx ry angle)))
    (draw-shape :triangle-fan vertices vertices)))

(defun star (n cx cy ra rb &optional (angle 0))
  (declare (type fixnum n)
	   (type real cx cy ra rb angle))
  (let ((vertices (mix-lists (ngon-vertices n cx cy ra ra (+ 90 angle))
			     (ngon-vertices n cx cy rb rb (- (+ 90 angle) (/ 180 n))))))
    (draw-shape :triangle-fan vertices vertices)))

(defun ellipse (cx cy rx ry)
  (declare (type real cx cy rx ry))
  (when (and (not (zerop rx)) (not (zerop ry)))
    (ngon (max 24 (truncate (* 5 (sqrt (/ (+ (abs rx) (abs ry)) 2))))) cx cy (abs rx) (abs ry))))

(defun circle (x y r)
  (declare (type real x y r))
  (when (not (zerop r))
    (ellipse x y (abs r) (abs r))))

(defun polygon (&rest coordinates)
  (draw-shape
   :triangles
   (triangulate coordinates)
   (group coordinates)))

(defun quadratic-bezier-point (v a b c)
  (let* ((d (lerp-lists v a b))
	 (e (lerp-lists v b c)))
    (lerp-lists v d e)))

(defun cubic-bezier-point (v a b c d)
  (let* ((e (lerp-lists v a b))
	 (f (lerp-lists v b c))
	 (g (lerp-lists v c d)))
    (quadratic-bezier-point v e f g)))

(defun bezier (x1 y1 bx1 by1 bx2 by2 x2 y2)
  (declare (type real x1 y1 bx1 by1 bx2 by2 x2 y2))
  (let ((a (list x1 y1))
	(b (list bx1 by1))
	(c (list bx2 by2))
	(d (list x2 y2))
	(cs (max 2 (pen-curve-steps (env-pen *env*)))))
    (apply #'polyline
	   (mapcan (lambda (v) (cubic-bezier-point v a b c d))
		   (alexandria:iota (1+ cs) :step (/ 1 cs))))))
