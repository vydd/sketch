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
     (r 6))
  (incf steps)
  (background (rgb 0.2 0.2 0.2))
  (let ((w width) (h height))
    (flet ((sin-calc (x) (sin (* +tau+ (/ (+ (/ steps 4) x) xs)))))
      (dotimes (x xs)
	(with-pen(make-pen :fill (rgb (/ (1+ (sin-calc x)) 2)
				      (/ (1+ (sin-calc (- x))) 2)
				      0.2))
	  (ngon 6 (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (sin-calc x))) r r)
	  (ngon 6 (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (sin-calc (- x)))) r r)
	  (ngon 6 (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (- (sin-calc (- x))))) r r)
	  (ngon 6 (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (- (sin-calc x)))) r r))))))
