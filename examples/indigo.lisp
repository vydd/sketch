;;;; indigo.lisp

(in-package #:sketch-examples)

;;;  ___ _   _ ____ ___ ____  ___
;;; |_ _| \ | |  _ \_ _/ ___|/ _ \
;;;  | ||  \| | | | | | |  _| | | |
;;;  | || |\  | |_| | | |_| | |_| |
;;; |___|_| \_|____/___\____|\___/

(defsketch indigo
    ((title "Indigo")
     (copy-pixels t)
     (a 0) (inc 20) (lx 200) (ly 200))
  (set-pen (make-pen :weight 2 :stroke +white+))
  (scale 0.5 0.5 200 200)
  (incf a inc)
  (rotate a 200 200)
  (line 0 0 lx ly)
  (rect -50 -50 100 100))

(defmethod setup ((indigo indigo) &key &allow-other-keys)
  (background +indigo+)
  (with-font (make-font :color +white+)
    (text "Click to redraw!" 10 10)))

(defmethod on-click ((indigo indigo) x y)
  (background +indigo+)
  (with-slots (inc lx ly) indigo
    (setf inc (+ (random 100) 1)
          lx (+ (random 100) 100)
          ly (+ (random 100) 100))))
