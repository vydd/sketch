;;;; complex-transforms.lisp

(in-package #:sketch)

;;; FIT, WITH-FIT
;;; Modes were taken from GTK, see https://docs.gtk.org/gtk4/enum.ContentFit.html
(defun fit (to-width to-height from-width from-height &key (mode :contain))
  (check-type mode (member :contain :cover :scale-down :fill))
  (ecase mode
    ((:contain :cover :scale-down)
     (flet ((%fit-scale (scale)
              (let ((x-shift (/ (- from-width
                                   (* to-width scale))
                                2))
                    (y-shift (/ (- from-height
                                   (* to-height scale))
                                2)))
                (translate x-shift y-shift)
                (scale scale))))
       (%fit-scale
        (ecase mode
          (:contain    (min (/ from-width to-width)
                            (/ from-height to-height)))
          (:cover      (max (/ from-width to-width)
                            (/ from-height to-height)))
          (:scale-down (min (/ from-width to-width)
                            (/ from-height to-height)
                            1))))))
    (:fill
     (scale (/ from-width to-width)
            (/ from-height to-height)))))

(defmacro with-fit ((to-width to-height from-width from-height &key (mode :contain))
                    &body body)
  `(with-current-matrix
       (fit ,to-width ,to-height ,from-width ,from-height :mode ,mode)
     ,@body))
