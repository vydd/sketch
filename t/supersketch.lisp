(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload 'sketch))

(defpackage :supersketch
  (:use :cl)
  (:local-nicknames (:s :sketch)))

(in-package supersketch)

(s:defsketch super1
    ((s:width 400)
     (s:height 400)
     (s:title "Supersketch 1 (shapes, colour, pens, image)")
     (pic (s:load-resource (s::relative-path "t/super1.png"))))
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
    (s:rect 130 85 20 40))
  (s:with-pen (:fill (s:color-filter-grayscale s:+indigo+ :average))
    (s:rect 150 85 20 40))
  (s:with-pen (:fill (s:color-filter-invert s:+red+))
    (s:rect 170 85 20 40))
  (s:with-pen (:fill (s:color-filter-rotate s:+red+))
    (s:rect 190 85 20 40))
  (s:with-pen (:fill (s:color-filter-hsb (s:hsb .5 .5 .5)
                                         :hue .1 :saturation .1 :brightness .1))
    (s:rect 210 85 20 40))
  (let ((y (- (/ s:height 2) 20)))
    (s:with-font (s:make-font :color s:+white+
                              :size 10
                              :line-height 1.2
                              :align :center)
      (s:text (format nil "Circle should be at center top of text box~%(Line 2) Image below should match (maybe system-dependent)")
              (/ s:width 2) y))
    (s:circle (/ s:width 2) y 5)
    (s:image pic 0 (+ y 60))))

(s:defsketch super2
    ((s:fullscreen t)
     (s:title "Supersketch 2 (fullscreen, image)")
     (sun (s:load-resource (s::relative-path "t/sun.png")))
     (cx (/ s:width 2))
     (cy (/ s:height 2))
     (swidth (s:image-width sun))
     (sheight (s:image-height sun))
     (pics
      (list (s:crop sun 0 0 (/ swidth 2) (/ sheight 2))
            (s:crop sun (/ swidth 2) 0 (/ swidth 2) (/ sheight 2))
            (s:crop sun 0 (/ swidth 2) (/ swidth 2) (/ sheight 2))
            (s:crop sun (/ swidth 2) (/ swidth 2) (/ swidth 2) (/ sheight 2))))
     (t0 0))
  (s:circle (/ s:width 2) (/ s:height 2) 50)
  (s:text "Circle should be in the middle Also testing image cropping." 0 0)
  (loop for pic in pics
        for (xmul ymul) in '((-1 -1) (1 -1) (-1 1) (1 1))
        do (s:image pic (+ cx (* xmul t0)) (+ cy (* ymul t0))))
  (setf t0 (mod (+ t0 .1) 40)))

(s:defsketch super3
    ((s:resizable t)
     (s:title "Supersketch 3 (resize, y-axis, text, close-on)")
     (s:y-axis :up)
     (s:close-on :space))
  (s:circle (/ s:width 2) (/ s:height 2) 50)
  (s:text "Circle should stay in middle during window resize." 0 30)
  (s:text "Exit with spacebar." 0 (/ s:height 2))
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

(s:defsketch super5
    ((s:title "Supersketch 5 (transforms, control flow))"))
  (s:with-font (s:make-font :size 14)
    (s:text "Red squares should get bigger, white squares should rotate."
            0 50))
  (s:with-identity-matrix
    (s:translate (/ s:width 2) (/ s:height 2))
    (s:with-pen (:weight 3 :stroke s:+white+)
      (loop repeat 10
            do (progn
                 (s:ngon 4 0 0 50 50)
                 (s:rotate (/ 360 10))))))
  (s:with-pen (:fill s:+red+)
    (dotimes (i 3)
      (let ((sfactor (+ 1 (/ i 3))))
        (s:with-translate ((* i 50) 0)
          (s:with-scale (sfactor sfactor)
            (s:rect 0 0 25 25)))))))

(s:defsketch super6
    ((s:title "Supersketch 6 (input, control flow, canvas)")
     (s:copy-pixels t)
     (cvs (let ((cvs (s:make-canvas 50 50)))
            (loop repeat 100
                  do (s:canvas-paint cvs s:+blue+ (random 50) (random 50)))
            cvs))
     (coords nil))
  (s:with-font (s:make-font :color s:+white+)
    (s:text "Press 'd' to add square." 0 0)
    (s:text "Click to draw canvas somewhere." 0 50))
  (if coords
      (when coords
        (s:draw cvs :x (first coords) :y (second coords))
        (setf coords nil))
      (s:rect (random s:width) (random s:height) 20 20))
  (s:stop-loop))

(defmethod s:on-key ((instance super6) (key (eql :d)) (state (eql :up)))
  (s:start-loop))

(defmethod s:on-click ((instance super6) x y)
  (setf (super6-coords instance) (list x y))
  (s:start-loop))
