;;;; stars.lisp

(in-package #:sketch-examples)

;;;  ____ _____  _    ____  ____
;;; / ___|_   _|/ \  |  _ \/ ___|
;;; \___ \ | | / _ \ | |_) \___ \
;;;  ___) || |/ ___ \|  _ < ___) |
;;; |____/ |_/_/   \_\_| \_\____/

(defsketch stars
    ((stars (loop :for i :below 10 :collect (make-stars)))
     (positions (loop :for i :from 18 :downto 0 :by 2 :collect i)))
  (background +black+)
  (dotimes (i (length stars))
    (incf (elt positions i) 0.03)
    (let ((zoom (get-zoom (elt positions i))))
      (with-current-matrix
        (with-pen (make-pen :fill (canvas-image (elt stars i)))
          (translate 200 200)
          (scale zoom)
          (rect -50 -50 100 100)))))
  (with-font (make-font :color +white+ :size 48
                        :align :center)
    (text "s k e t c h" 200 160))
  (when (>= (get-zoom (car positions)) 20)
    (setf (car positions) 0)
    (setf positions (rotate-list positions))
    (setf stars (rotate-list stars))))

(defun make-stars ()
  (let ((canvas (make-canvas 100 100)))
    (dotimes (i 20)
      (let ((x (random 100))
            (y (random 100)))
        (unless (and (< 40 x 60)
                     (< 40 y 60)))
        (canvas-paint canvas (gray-255 (+ 200 (random 55))) x y)))
    (canvas-lock canvas)
    canvas))

(defun rotate-list (list)
  (let ((el (pop list)))
    (reverse (cons el (reverse list)))))

(defun get-zoom (position)
  (exp (/ position 6)))
