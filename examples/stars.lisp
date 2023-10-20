;;;; stars.lisp

(in-package #:sketch-examples)

;;;  ____ _____  _    ____  ____
;;; / ___|_   _|/ \  |  _ \/ ___|
;;; \___ \ | | / _ \ | |_) \___ \
;;;  ___) || |/ ___ \|  _ < ___) |
;;; |____/ |_/_/   \_\_| \_\____/

(defsketch stars
    ((bw nil)
     (stars (loop :for i :below 10 :collect (make-stars bw)))
     (positions (loop :for i :from 18 :downto 0 :by 2 :collect i))
     (rotations (loop :repeat 10 :collect (cons 0 (- (random 0.2) 0.05)))))
  (background +black+)
  (dotimes (i (length stars))
    (incf (elt positions i) 0.03)
    (incf (car (elt rotations i))
          (cdr (elt rotations i)))
    (let ((zoom (get-zoom (elt positions i)))
          (rotation (car (elt rotations i))))
      (with-current-matrix
        (with-pen (make-pen :fill (canvas-image (elt stars i)))
          (translate 200 200)
          (scale zoom)
          (rotate rotation)
          (rect -50 -50 100 100)))))
  (with-font (make-font :color +white+ :size 48
                        :align :center)
    (text "s k e t c h" 200 160))
  (when (>= (get-zoom (car positions)) 20)
    (setf (car positions) 0)
    (setf positions (rotate-list positions)
          rotations (rotate-list rotations))
    (setf stars (rotate-list stars))))

(defun make-stars (bw)
  (let ((canvas (make-canvas 100 100)))
    (dotimes (i 20)
      (let ((x (random 100))
            (y (random 100)))
        (unless (and (< 40 x 60)
                     (< 40 y 60)))
        (canvas-paint canvas (if bw
				 (gray-255 (+ 200 (random 55)))
				 (if (< (random 3) 1)
				     +magenta+
				     +cyan+))
                      x y)))
    (canvas-lock canvas)
    canvas))

(defun rotate-list (list)
  (let ((el (pop list)))
    (reverse (cons el (reverse list)))))

(defun get-zoom (position)
  (exp (/ position 6)))
