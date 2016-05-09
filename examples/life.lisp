;;;; life.lisp

(in-package #:sketch-examples)

;;;  _     ___ _____ _____
;;; | |   |_ _|  ___| ____|
;;; | |    | || |_  |  _|
;;; | |___ | ||  _| | |___
;;; |_____|___|_|   |_____|

;;; Press any key to toggle between editing and iterating.
;;; When in edit mode, click on cells to toggle them.

(defsketch life
    ((title "Conway's Game of Life")
     (columns 30)
     (rows 30)
     (cell-size 15)
     (width (* columns cell-size))
     (height (* rows cell-size))
     (cells (make-array `(,(+ 2 rows) ,(+ 2 columns) 2)
			:initial-element 0
			:element-type '(mod 2)))
     (front 0)
     (color-bg (gray 0.2))
     (pen-dead (make-pen :fill (gray 0)))
     (pen-alive (make-pen :fill (gray 0.5)))
     (running nil))
  (labels ((neighbors (x y)
	     (let ((acc 0))
	       (dotimes (i 3)
		 (dotimes (j 3)
		   (setf acc (+ acc (aref cells (+ i y) (+ j x) front)))))
	       (- acc (aref cells (1+ y) (1+ x) front))))
	   (alivep (x y)
	     (= 1 (aref cells (1+ y) (1+ x) front)))
	   (next-state (x y)
	     (let ((alive (alivep x y)) (neighbors (neighbors x y)))
	       (if (or (and alive (<= 2 neighbors 3))
		       (and (not alive) (= 3 neighbors)))
		   1 0))))
    (background color-bg)
    (dotimes (y rows)
      (dotimes (x columns)
	(with-pen (if (zerop (aref cells (1+ y) (1+ x) front))
		      pen-dead
		      pen-alive)
	  (ellipse (+ (/ cell-size 2) (* x cell-size))
		   (+ (/ cell-size 2) (* y cell-size))
		   (/ cell-size 3)
		   (/ cell-size 3)))
	(setf (aref cells (1+ y) (1+ x) (mod (1+ front) 2))
	      (next-state x y))))
    (when running
      (setf front (mod (1+ front) 2)))))

(defmethod kit.sdl2:textinput-event ((window life) ts text)
  (with-slots (running) window
    (setf running (not running))))

(defmethod kit.sdl2:mousebutton-event ((window life) state ts b x y)
  (when (eq state :mousebuttondown)
    (with-slots (cells front running cell-size) window
      (when (not running)
	(let ((cy (1+ (truncate (/ y cell-size))))
	      (cx (1+ (truncate (/ x cell-size)))))
	  (setf (aref cells cy cx front)
		(mod (1+ (aref cells cy cx front)) 2)))))))
