;;;; hello-world.lisp

(in-package #:sketch-examples)

;;;  _   _ _____ _     _     ___   __        _____  ____  _     ____
;;; | | | | ____| |   | |   / _ \  \ \      / / _ \|  _ \| |   |  _ \
;;; | |_| |  _| | |   | |  | | | |  \ \ /\ / / | | | |_) | |   | | | |
;;; |  _  | |___| |___| |__| |_| |   \ V  V /| |_| |  _ <| |___| |_| |
;;; |_| |_|_____|_____|_____\___/     \_/\_/  \___/|_| \_\_____|____/

(defsketch hello-world
    ((title "Hello, world!")
     (unit (/ width 10))
     (height width))
  (background (gray 0.6))
  (with-pen (make-pen :fill (rgb 0.380 0.695 0.086) :stroke (rgb 1 1 0) :weight 4)
    (polygon (* 5 unit) unit unit (* 9 unit) (* 9 unit) (* 9 unit))
    (text title 20 20)))
