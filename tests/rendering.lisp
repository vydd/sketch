;;;; tests/rendering.lisp

(in-package #:sketch-tests)

(def-suite rendering-tests
  :description "Tests for rendering shapes"
  :in sketch-tests)

(in-suite rendering-tests)

;;; Rendering test infrastructure

(defparameter *render-done* nil)
(defparameter *render-output-path* nil)
(defparameter *render-frames* 5)

(defmacro with-render-test ((name width height) &body draw-body)
  "Execute DRAW-BODY in a sketch context and save the result as a PNG.
   Returns the path to the saved PNG."
  (let ((sketch-name (gensym "RENDER-TEST-")))
    `(progn
       (setf *render-done* nil)
       (setf *render-output-path*
             (format nil "/tmp/sketch-test-~a-~a.png"
                     ,name (get-universal-time)))
       (sketch:defsketch ,sketch-name
           ((sketch:width ,width)
            (sketch:height ,height)
            (frame-count 0))
         ,@draw-body
         (incf frame-count)
         (when (= frame-count *render-frames*)
           (sketch:save-png *render-output-path*)
           (setf *render-done* t)
           (sdl2:push-event :quit)))
       (make-instance ',sketch-name)
       (loop until *render-done* do (sleep 0.05))
       *render-output-path*)))

;;; Shape rendering tests

(test render-all-shapes
  "Test that all basic shapes render without error"
  (let ((output-path
          (with-render-test ("all-shapes" 800 600)
            (sketch:background sketch:+black+)

            ;; Row 1: point, line, polyline
            (sketch:with-pen (sketch:make-pen :stroke sketch:+white+ :weight 5)
              (sketch:point 50 50))

            (sketch:with-pen (sketch:make-pen :stroke sketch:+red+ :weight 3)
              (sketch:line 100 30 200 70))

            (sketch:with-pen (sketch:make-pen :stroke sketch:+green+ :weight 2)
              (sketch:polyline 250 30 300 70 350 30 400 70))

            ;; Row 2: rect, ngon, star
            (sketch:with-pen (sketch:make-pen :fill sketch:+blue+)
              (sketch:rect 50 100 80 60))

            (sketch:with-pen (sketch:make-pen :fill sketch:+yellow+)
              (sketch:ngon 6 220 130 40 40))

            (sketch:with-pen (sketch:make-pen :fill sketch:+magenta+)
              (sketch:star 5 350 130 50 25))

            ;; Row 3: ellipse, circle, polygon
            (sketch:with-pen (sketch:make-pen :fill sketch:+cyan+)
              (sketch:ellipse 90 250 60 35))

            (sketch:with-pen (sketch:make-pen :fill sketch:+orange+)
              (sketch:circle 220 250 40))

            (sketch:with-pen (sketch:make-pen :fill sketch:+white+)
              (sketch:polygon 300 210 380 210 390 280 340 300 290 280))

            ;; Row 4: bezier curve
            (sketch:with-pen (sketch:make-pen :stroke sketch:+green+ :weight 3 :curve-steps 50)
              (sketch:bezier 50 350 150 320 250 450 350 350))

            ;; Row 5: shapes with stroke only
            (sketch:with-pen (sketch:make-pen :stroke sketch:+red+ :fill nil :weight 2)
              (sketch:rect 50 420 80 60)
              (sketch:circle 220 450 40)
              (sketch:ngon 5 350 450 40 40))

            ;; Row 6: shapes with both fill and stroke
            (sketch:with-pen (sketch:make-pen :stroke sketch:+white+ :fill sketch:+blue+ :weight 2)
              (sketch:rect 450 100 80 60)
              (sketch:circle 580 130 40)
              (sketch:ngon 8 700 130 40 40)))))

    (is (probe-file output-path))
    (let ((size (with-open-file (f output-path) (file-length f))))
      (is (> size 1000)))))  ; PNG should be reasonably sized

(test render-rect-variations
  "Test rectangle rendering with different parameters"
  (let ((output-path
          (with-render-test ("rect-variations" 400 400)
            (sketch:background sketch:+black+)

            ;; Different sizes
            (sketch:with-pen (sketch:make-pen :fill sketch:+red+)
              (sketch:rect 10 10 50 50)
              (sketch:rect 70 10 100 30)
              (sketch:rect 180 10 30 80))

            ;; With stroke
            (sketch:with-pen (sketch:make-pen :fill sketch:+blue+ :stroke sketch:+white+ :weight 2)
              (sketch:rect 10 120 80 80)
              (sketch:rect 100 120 80 80)))))

    (is (probe-file output-path))))

(test render-circle-variations
  "Test circle and ellipse rendering with different parameters"
  (let ((output-path
          (with-render-test ("circle-variations" 400 400)
            (sketch:background sketch:+black+)

            ;; Different sized circles
            (sketch:with-pen (sketch:make-pen :fill sketch:+green+)
              (sketch:circle 50 50 20)
              (sketch:circle 120 50 35)
              (sketch:circle 220 60 50))

            ;; Ellipses
            (sketch:with-pen (sketch:make-pen :fill sketch:+cyan+)
              (sketch:ellipse 60 150 50 25)
              (sketch:ellipse 180 150 25 50)
              (sketch:ellipse 280 150 60 40))

            ;; With stroke
            (sketch:with-pen (sketch:make-pen :fill sketch:+yellow+ :stroke sketch:+red+ :weight 3)
              (sketch:circle 100 280 60)
              (sketch:ellipse 280 280 70 40)))))

    (is (probe-file output-path))))

(test render-ngon-variations
  "Test ngon rendering with different number of sides"
  (let ((output-path
          (with-render-test ("ngon-variations" 600 200)
            (sketch:background sketch:+black+)

            ;; Triangle through octagon
            (loop for n from 3 to 8
                  for x from 50 by 80
                  for color in (list sketch:+red+ sketch:+orange+ sketch:+yellow+
                                    sketch:+green+ sketch:+cyan+ sketch:+blue+)
                  do (sketch:with-pen (sketch:make-pen :fill color)
                       (sketch:ngon n x 100 35 35))))))

    (is (probe-file output-path))))

(test render-star-variations
  "Test star rendering with different parameters"
  (let ((output-path
          (with-render-test ("star-variations" 500 200)
            (sketch:background sketch:+black+)

            ;; Stars with different number of points
            (sketch:with-pen (sketch:make-pen :fill sketch:+yellow+)
              (sketch:star 4 60 100 50 25)
              (sketch:star 5 170 100 50 25)
              (sketch:star 6 280 100 50 25)
              (sketch:star 8 390 100 50 20)))))

    (is (probe-file output-path))))

(test render-polygon-complex
  "Test polygon rendering with complex shapes"
  (let ((output-path
          (with-render-test ("polygon-complex" 400 400)
            (sketch:background sketch:+black+)

            ;; Simple triangle
            (sketch:with-pen (sketch:make-pen :fill sketch:+red+)
              (sketch:polygon 50 50 150 50 100 150))

            ;; Pentagon
            (sketch:with-pen (sketch:make-pen :fill sketch:+green+)
              (sketch:polygon 250 50 350 80 330 180 270 180 220 80))

            ;; Arrow shape
            (sketch:with-pen (sketch:make-pen :fill sketch:+blue+)
              (sketch:polygon 50 250 150 300 50 350 80 300))

            ;; L-shape
            (sketch:with-pen (sketch:make-pen :fill sketch:+yellow+)
              (sketch:polygon 200 220 280 220 280 280 240 280 240 350 200 350)))))

    (is (probe-file output-path))))

(test render-bezier-curves
  "Test bezier curve rendering"
  (let ((output-path
          (with-render-test ("bezier-curves" 400 300)
            (sketch:background sketch:+black+)

            ;; S-curve
            (sketch:with-pen (sketch:make-pen :stroke sketch:+red+ :weight 3 :curve-steps 30)
              (sketch:bezier 50 50 150 50 50 150 150 150))

            ;; Arch
            (sketch:with-pen (sketch:make-pen :stroke sketch:+green+ :weight 3 :curve-steps 30)
              (sketch:bezier 200 150 200 50 350 50 350 150))

            ;; Loop-like
            (sketch:with-pen (sketch:make-pen :stroke sketch:+cyan+ :weight 2 :curve-steps 50)
              (sketch:bezier 50 200 200 180 0 280 150 250)))))

    (is (probe-file output-path))))

(test render-line-weights
  "Test line rendering with different weights"
  (let ((output-path
          (with-render-test ("line-weights" 400 300)
            (sketch:background sketch:+black+)

            (loop for weight from 1 to 10
                  for y from 20 by 25
                  do (sketch:with-pen (sketch:make-pen :stroke sketch:+white+ :weight weight)
                       (sketch:line 50 y 350 y))))))

    (is (probe-file output-path))))

(test render-colors
  "Test color rendering accuracy"
  (let ((output-path
          (with-render-test ("colors" 400 200)
            (sketch:background sketch:+black+)

            ;; Primary colors
            (sketch:with-pen (sketch:make-pen :fill sketch:+red+)
              (sketch:rect 10 10 50 80))
            (sketch:with-pen (sketch:make-pen :fill sketch:+green+)
              (sketch:rect 70 10 50 80))
            (sketch:with-pen (sketch:make-pen :fill sketch:+blue+)
              (sketch:rect 130 10 50 80))

            ;; Secondary colors
            (sketch:with-pen (sketch:make-pen :fill sketch:+yellow+)
              (sketch:rect 10 100 50 80))
            (sketch:with-pen (sketch:make-pen :fill sketch:+cyan+)
              (sketch:rect 70 100 50 80))
            (sketch:with-pen (sketch:make-pen :fill sketch:+magenta+)
              (sketch:rect 130 100 50 80))

            ;; Grayscale
            (sketch:with-pen (sketch:make-pen :fill sketch:+white+)
              (sketch:rect 200 10 50 80))
            (sketch:with-pen (sketch:make-pen :fill sketch:+gray+)
              (sketch:rect 260 10 50 80))
            (sketch:with-pen (sketch:make-pen :fill sketch:+black+)
              (sketch:rect 320 10 50 80))

            ;; Custom colors
            (sketch:with-pen (sketch:make-pen :fill (sketch:rgb 1.0 0.5 0.0))
              (sketch:rect 200 100 50 80))
            (sketch:with-pen (sketch:make-pen :fill (sketch:hsb 0.8 1.0 1.0))
              (sketch:rect 260 100 50 80))
            (sketch:with-pen (sketch:make-pen :fill (sketch:gray 0.3))
              (sketch:rect 320 100 50 80)))))

    (is (probe-file output-path))))
