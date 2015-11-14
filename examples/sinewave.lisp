;;;; sinewave.lisp

(in-package #:sketch-examples)

;;;  ____ ___ _   _ _______        _____     _______
;;; / ___|_ _| \ | | ____\ \      / / \ \   / / ____|
;;; \___ \| ||  \| |  _|  \ \ /\ / / _ \ \ / /|  _|
;;;  ___) | || |\  | |___  \ V  V / ___ \ V / | |___
;;; |____/___|_| \_|_____|  \_/\_/_/   \_\_/  |_____|

(defsketch sinewave
    (:title "Sinewave" :width 400 :height 400 :debug :scancode-f1)  
    ((steps 0)
     (xs 40)
     (pen (make-pen :fill (gray 1.0))))
  #|
  (incf steps)
  (background (gray 0.2))
  (with-pen pen   
    (let ((w width) (h height))
      (flet ((sin-calc (x) (sin (* +tau+ (/ (+ (/ steps 4) x) xs)))))
	(dotimes (x xs)
	  (ellipse (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (sin-calc x)))
		   (/ w xs 3) (/ w xs 3)))))))
  |#)

