;;;; package.lisp

(uiop:define-package #:sketch
    (:use #:cl)
  (:import-from :kit.sdl2
                :mousebutton-event
                :mousemotion-event
                :mousewheel-event
                :textinput-event
                :keyboard-event
                :other-event
                :close-window)
  (:export :sketch
           :setup
           :draw

           :defsketch

           :sketch-title
           :sketch-width
           :sketch-height
           :sketch-fullscreen
           :sketch-copy-pixels
           :sketch-y-axis

           :title
           :width
           :height
           :fullscreen
           :copy-pixels
           :y-axis

           ;; Math
           :clamp-1
           :normalize

           :+pi+
           :+two-pi+
           :+tau+
           :+half-pi+
           :+quarter-pi+
           :+epsilon+
           :+phi+
           :+golden-ratio+
           :+e+

           :radians
           :degrees

           ;; Utils
           :relative-path

           ;; Colors
           :color
           :make-color
           :color-red
           :color-green
           :color-blue
           :color-hue
           :color-saturation
           :color-brightness
           :color-alpha
           :rgb-to-hsb
           :hsb-to-rgb
           :rgb
           :hsb
           :gray
           :rgb-255
           :hsb-360
           :gray-255
           :hex-to-color
           :color-rgb
           :color-rgba
           :color-hsba
           :color-vector
           :color-vector-255
           :lerp-color
           :random-color
           :hash-color
           :color-filter-grayscale
           :color-filter-invert
           :color-filter-rotate
           :color-filter-hsb
           :+red+
           :+green+
           :+blue+
           :+yellow+
           :+magenta+
           :+cyan+
           :+orange+
           :+white+
           :+black+

           ;; Pen
           :pen
           :pen-stroke
           :pen-fill
           :pen-weight
           :make-pen
           :set-pen
           :copy-pen
           :flip-pen
           :with-pen
           :background

           ;; Shapes
           :point
           :line
           :polyline
           :rect
           :ngon
           :star
           :ellipse
           :circle
           :polygon
           :bezier

           ;; Transforms
           :set-matrix
           :push-matrix
           :pop-matrix
           :translate
           :rotate
           :scale
           :with-matrix
           :with-identity-matrix
           :with-current-matrix

           ;; Channels
           :register-input
           :in
           :out
           :define-channel-observer
           :define-named-channel-observer
           :reset-all-channels

           ;; Figures
           :deffigure

           ;; Resources
           :load-resource
           :image

           ;; Font
           :make-font
           :with-font
           :set-font
           :text
           ))
