;;;; complex-transforms.lisp

(in-package #:sketch)

;;; FIT, WITH-FIT
;;; Modes were taken from GTK, see https://docs.gtk.org/gtk4/enum.ContentFit.html
(defun fit (to-width to-height from-width from-height
            &key (to-x 0) (to-y 0) (from-x 0) (from-y 0)
                 (mode :contain))
  (declare (type (member :contain :cover :scale-down :fill) mode))
  (translate from-x from-y)
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
            (/ from-height to-height))))
  (translate (- to-x) (- to-y)))

(defmacro with-fit (((  to-width   to-height &optional (  to-x 0) (  to-y 0))
                     (from-width from-height &optional (from-x 0) (from-y 0))
                     &key (mode :contain))
                    &body body)
  (alexandria:once-only (to-width to-height to-x to-y from-width from-height from-x from-y mode)
    `(with-current-matrix
       (fit ,to-width ,to-height ,from-width ,from-height
            :to-x ,to-x :to-y ,to-y :from-x ,from-x :from-y ,from-y
            :mode ,mode)
       ,@body)))
