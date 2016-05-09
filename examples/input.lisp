;;; input.lisp

(in-package #:sketch-examples)

;;  ___ _   _ ____  _   _ _____
;; |_ _| \ | |  _ \| | | |_   _|
;;  | ||  \| | |_) | | | | | |
;;  | || |\  |  __/| |_| | | |
;; |___|_| \_|_|    \___/  |_|

;;; WIP

(defsketch input
    ((title "Input")
     (x 0) (y 0) (w 10) (h 10) (r 0) (c 0))
  (background (rgb 1 1 1))
  (translate (+ 200 (- x (/ w 2))) (+ 200 (- y (/ h 2))))
  (rotate r (/ w 2) (/ h 2))
  (with-pen (make-pen :fill (rgb-255 c 40 40))
    (rect 0 0 w h)))

(defmethod kit.sdl2:controller-axis-motion-event ((win ctest) controller timestamp axis value)
  (with-slots (x y w h r c) win
    (case axis
      (0 (setf x (/ value 100)))
      (1 (setf y (/ value 100)))
      (2 (setf w (max 50 (abs (/ value 100)))))
      (3 (setf h (max 50 (abs (/ value 100)))))
      (4 (setf r (abs (/ value 100))))
      (5 (setf c (min (/ value 100) 255))))))

(defmethod kit.sdl2:controller-button-event ((win ctest) controller state timestamp button)
  (when (eql state :controllerbuttonup)
    (format t "(B ~a ~a ~a) " button state (type-of button))
    (finish-output)))
