;;;; tetris.lisp

(in-package #:sketch-examples)

;;;  _____ _____ _____ ____  ___ ____
;;; |_   _| ____|_   _|  _ \|_ _/ ___|
;;;   | | |  _|   | | | |_) || |\___ \
;;;   | | | |___  | | |  _ < | | ___) |
;;;   |_| |_____| |_| |_| \_\___|____/

(defparameter *tetrominos*
  '())




(defsketch tetris
    (:title "Tetris" :height 600 :width 300)
    ()
  (background (rgb 1 1 0)))



(make-instance 'tetris)
(background )
