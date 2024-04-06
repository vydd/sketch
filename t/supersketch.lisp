(defpackage :supersketch
  (:use :cl)
  (:local-nicknames (:s :sketch)))

(in-package supersketch)

(s:defsketch super1
    ((s:width 600)
     (s:height 400)
     (s:title "Supersketch 1 (shapes, colour, pens)"))
  (s:background (s:gray 0.6))
  (s:rect 0 0 25 25)
  (s:with-pen (s:make-pen :fill s:+red+ :stroke s:+blue+)
    (s:circle 45 20 20))
  (s:with-pen (:weight 0 :fill (s:rgb 0 .8 .8))
    (s:ellipse 105 30 40 30))
  (s:with-pen (:weight 5 :stroke (s:hsb .1 .8 .6 .9))
    (s:line 145 25 180 25))
  (s:with-pen (:stroke (s:hash-color 10))
    (s:polyline 145 35 180 35 160 50))
  (s:with-pen (:fill (s:rgb-255 120 120 200))
    (s:polygon 185 20 250 25 200 50))
  (s:with-pen (:fill (s:hsb-360 300 50 80 200) :stroke (s:gray-255 20))
    (s:ngon 3 275 25 25 25))
  (s:with-pen (:fill (s:hex-to-color "#4bcdef"))
    (s:ngon 4 325 25 25 25))
  (s:with-pen (:fill (s:lerp-color s:+red+ s:+blue+ .5))
    (s:ngon 5 375 25 25 25))
  (s:with-pen (:stroke (s:random-color))
    (s:bezier 0 75 25 100 50 60 125 75))
  (s:with-pen (s:make-pen :stroke s:+yellow+ :weight 4 :curve-steps 5)
    (s:bezier 0 85 25 110 50 70 125 85))
  (s:with-pen (:fill (s:color-filter-grayscale s:+red+))
    (s:rect 0 150 20 40))
  (s:with-pen (:fill (s:color-filter-grayscale s:+indigo+ :average))
    (s:rect 20 150 20 40))
  (s:with-pen (:fill (s:color-filter-invert s:+red+))
    (s:rect 40 150 20 40))
  (s:with-pen (:fill (s:color-filter-rotate s:+red+))
    (s:rect 60 150 20 40))
  (s:with-pen (:fill (s:color-filter-hsb (s:hsb .5 .5 .5)
                                         :hue .1 :saturation .1 :brightness .1))
    (s:rect 80 150 20 40)))

(s:defsketch super2
    ((s:fullscreen t)
     (s:title "Supersketch 2 (fullscreen)")
  (s:circle (/ s:width 2) (/ s:height 2) 50)
  (s:text "Circle should be in the middle." 0 0)))

(s:defsketch super3
    ((s:resizable t)
     (s:title "Supersketch 3 (resize, y-axis, text, close-on)")
     (s:y-axis :up)
     (s:close-on :space))
  (s:circle (/ s:width 2) (/ s:height 2) 50)
  (s:text "Circle should stay in middle during window resize." 0 30)
  (s:text "Exit with spacebar" 0 (/ s:height 2))
  (s:ngon 4 10 10 10 10)
  (s:with-font (s:make-font :color s:+white+ :size 10)
    (s:text "Square should be in bottom left." 0 (- s:height 20))))

(s:defsketch super4
    ((s:copy-pixels t)
     (s:title "Supersketch 4 (copy-pixels)")
     (i 0))
  ;; There should be 1 black circle (otherwise, first draw call has been
  ;; overwritten somehow).
  (s:with-pen (:fill (if (= 0 i) s:+black+ s:+white+))
    (s:circle (random s:width) (random s:height) 20))
  (incf i))

(defmethod s:setup ((instance super4) &key &allow-other-keys)
  (s:background s:+blue+))

;; TODO (see readme):
;; - transforms
;; - text / font
;; - images
;; - input
;; - control flow
