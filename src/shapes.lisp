;;;; shapes.lisp

(in-package #:sketch)

;;;  ____  _   _    _    ____  _____ ____
;;; / ___|| | | |  / \  |  _ \| ____/ ___|
;;; \___ \| |_| | / _ \ | |_) |  _| \___ \
;;;  ___) |  _  |/ ___ \|  __/| |___ ___) |
;;; |____/|_| |_/_/   \_\_|   |_____|____/


(defun grow-polygon (vertices distance)
  (let ((edges
	 (loop
	    for i in (append (last vertices) (butlast vertices) ())
	    for j in vertices
	    collect (list i j))))    
    edges))

(grow-polygon '((1 1) (2 3) (3 5) (4 6)))

(let ((vertices '((1 1) (2 3) (3 5) (4 6))))
  (append (last vertices) (butlast vertices) ()))

;;; Meshes

(defun make-meshes (vertices)
  (static-vectors:make-static-vector
   (* 2 (length vertices))
   :element-type 'single-float
   :initial-contents (apply #'append vertices)))

(defmacro define-mesh (name arglist vertices &body body)
  (let ((mesh (gensym)))
    `(let ((,mesh (make-meshes ,@body)))
       (defun ,name ,arglist
	 ))))

(define-mesh rect ())


;; (defparameter *sketch-mesh-rectangle*
;;   (define-mesh '((-0.5 -0.5)
;; 		 (-0.5 0.5)
;; 		 (0.5 0.5)
;; 		 (0.5 -0.5))))




;; (defparameter *sketch-mesh-ellipse*
;;   (define-mesh 
;;       )))



;; (define-mesh sketch-mesh-rectangle
;;   (-0.5 -0.5)
;;   (-0.5 0.5)
;;   (0.5 0.5)
;;   (0.5 -0.5))

;; (define-mesh sketch-mesh-ellipse
;;   )

;; (define-mesh sketch-mesh-ngon)



;; (defun draw-mesh (mesh)
;;   (let ((stroke (pen-stroke (env-pen *env*)))
;; 	(fill (pen-fill (env-pen *env*))))
;;     (when stroke
;;       (draw-mesh-stroke mesh stroke))
;;     (when fill
;;       (draw-mesh-fill mesh fill))))

;; (defun draw-polygon (mesh)
;;   ()
;;   )



;;; 2D Primitives




(defmethod draw-sketch-primitive (primitive)
  


  
  )


(defun line (x1 y1 x2 y2)  
  ;(declare (optimize (speed 3) (safety 0) (debug 0)))
  ;; TODO: Make this sane.
  (when (pen-stroke (env-pen *env*))
    (let* ((a (atan (- y2 y1) (- x2 x1)))
	   (w (/ (or (pen-weight (env-pen *env*)) 1) 2))
	   (dx (* 2 (* (sin a) w)))
	   (dy (* 2 (* (cos a) w))))
      (push-indices 0 1 2 2 1 3)
      (push-vertices (x1 (+ y1 dy))
		     (x2 (+ y2 dy))
		     ((+ x1 dx) y1)		     
		     ((+ x2 dx) y2))
      (push-stroke 4))))

(defun ellipse (a b c d &key (mode :corner))
  ;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (ngon (max 22 (truncate (* 5 (sqrt (/ (+ c d) 2))))) a b c d :mode mode))

(defun point (x y)
  ;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (pen-stroke (env-pen *env*))
    (push-indices 0 0 0)
    (push-vertices (x y))
    (push-stroke 1)))

(defun rect (a b c d &key (mode :corner))
  ;(declare (optimize (speed 3) (safety 0 (debug 0))))
  (when (pen-fill (env-pen *env*))
    (push-indices 0 1 2 0 2 3)
    (push-vertices (a b)
		   (a (+ b d))
		   ((+ a c) (+ b d))
		   ((+ a c) b))
    (push-fill 4))
  (when (pen-stroke (env-pen *env*))
    (line a b a (+ b d))
    (line a (+ b d) (+ a c) (+ b d))
    (line (+ a c) (+ b d) (+ a c) b)
    (line (+ a c) b a b)))

(defun ngon (n a b c d &key (mode :corner) (angle 0))
  ;; http://slabode.exofire.net/circle_draw.shtml
  (let* ((c (if (zerop c) +epsilon+ c))
	 (theta (/ +tau+ n))
	 (tangential (tan theta))
	 (radial (cos theta))
	 (x c)
	 (y 0)
	 (y-mul (/ d c)))
    (when (pen-fill (env-pen *env*))
      (push-vertices (a b))
      (push-fill 1)
      (dotimes (i n)
	(push-vertices ((+ x a) (+ (* y-mul y) b)))
	(psetf x (* radial (- x (* (- y) tangential)))
	       y (* radial (- y (* x tangential))))
	(push-fill 1))
      (dotimes (i n)
	(push-indices (- 0 n 1) (- i n) (- (mod (+ i 1) n) n))))
    
    (when (pen-stroke (env-pen *env*))
      (dotimes (i n)
	(let ((next-x (* radial (+ x (* (- y) tangential))))
	      (next-y (* radial (+ y (* x tangential)))))
	  (line (+ x a) (+ (* y-mul y) b)
		(+ next-x a) (+ (* y-mul next-y) b))
	  (setf x next-x
		y next-y))))))

(defun triangle (x1 y1 x2 y2 x3 y3)  
  ;(declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (pen-fill (env-pen *env*))
    (push-indices 0 1 2)
    (push-vertices (x1 y1) (x2 y2) (x3 y3))
    (push-fill 3))
  (when (pen-stroke (env-pen *env*))
    (line x1 y1 x2 y2)
    (line x2 y2 x3 y3)
    (line x3 y3 x1 y1)))


