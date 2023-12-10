(in-package #:sketch-examples)

;; Regenerate the sketch, drawing a circle in a new place, every
;; time there's a click.

(defsketch control-flow
    ((title "Control Flow")
     (width 400)
     (height 400))
  (circle (random width) (random height) 20)
  (stop-loop))

(defmethod on-click ((sketch control-flow) x y)
  (start-loop))
