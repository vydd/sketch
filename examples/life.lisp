;;;; life.lisp

(in-package #:sketch-examples)

;;;  _     ___ _____ _____
;;; | |   |_ _|  ___| ____|
;;; | |    | || |_  |  _|
;;; | |___ | ||  _| | |___
;;; |_____|___|_|   |_____|

;;; DOES NOT RUN. WORK IN PROGRESS.

(defsketch life
    (:title "Conway's Game of Life" :width 800 :height 600 :framerate :auto)
    ((cols 60) (rows 80) (cell-size 10)
     (cells (make-array '(80 60) :initial-element 0))
     (color-bg (gray 0.2))
     (pen-dead (make-pen :fill (gray 0)))
     (pen-alive (make-pen :fill (gray 1))))
  (labels ((neighbors (x y)
	     (let ((mx 79) (my 59))
	       (+ (if (> x 0) (aref cells (- x 1) y) 0)
		  (if (and (> x 0) (> y 0)) (aref cells (- x 1) (- y 1)) 0)
		  (if (and (> x 0) (< y my)) (aref cells (- x 1) (+ y 1)) 0)
		  (if (< x mx) (aref cells (+ x 1) y) 0)
		  (if (and (< x mx) (> y 0)) (aref cells (+ x 1) (- y 1)) 0)
		  (if (and (< x mx) (< y my)) (aref cells (+ x 1) (+ y 1)) 0)
		  (if (> y 0) (aref cells x (- y 1)) 0)
		  (if (< y my) (aref cells x (+ y 1)) 0))))
	   (alivep (x y)
	     (= 1 (aref cells x y)))
	   (next-state (x y)
	     (let ((alive (alivep x y)) (neighbors (neighbors x y)))
	       (cond ((and alive (< neighbors 2)) 0)
		     ((and alive (<= neighbors 2 3)) 1)
		     ((and alive (> neighbors 3)) 0)
		     (t 1)))))
    (background color-bg)
    (let ((next-cells (make-array `(,rows ,cols))))
      (dotimes (row rows)
	(dotimes (col cols)
	  (with-pen pen-alive
	    (ellipse (+ (/ cell-size 2) (* row cell-size))
		     (+ (/ cell-size 2) (* col cell-size))
		     (/ cell-size 2) (/ cell-size 2)))
	  (setf (aref next-cells row col) (next-state row col))))
      (setf cells next-cells))))
