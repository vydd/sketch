(in-package #:sketch-examples)

;; Regenerate the sketch, drawing a circle in a new place, every
;; time there's a click.

(defsketch controlflow
    ((title "Control Flow")
     (width 400)
     (height 400))
  (circle (random width) (random height))
  (loop-no))

(defmethod onclick ((sketch controlflow) x y)
  (loop-yes))
