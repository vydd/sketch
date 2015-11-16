;;;; sinewave.lisp

(in-package #:sketch-examples)

;;;  ____ ___ _   _ _______        _____     _______
;;; / ___|_ _| \ | | ____\ \      / / \ \   / / ____|
;;; \___ \| ||  \| |  _|  \ \ /\ / / _ \ \ / /|  _|
;;;  ___) | || |\  | |___  \ V  V / ___ \ V / | |___
;;; |____/___|_| \_|_____|  \_/\_/_/   \_\_/  |_____|

(defsketch sinewave
    (:title "Sinewave" :width 1200 :height 600 :debug :scancode-f1)  
    ((steps 0)
     (xs 800)
     (r 80)
     (pen (make-pen :fill (gray 1.0))))
  (incf steps)
  (background (rgb 0.2 0.6 0.8))
  (with-pen (make-pen :fill (gray 1.0))
    (let ((w width) (h height))
      (flet ((sin-calc (x) (sin (* +tau+ (/ (+ (/ steps 4) x) xs)))))
  	(dotimes (x xs)
  	  (rect (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (sin-calc x))) r r)
  	  (rect (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (sin-calc (- x)))) r r)
	  (rect (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (- (sin-calc x)))) r r)
	  (rect (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (- (sin-calc (- x))))) r r))))))
