;;;; package.lisp

(defpackage #:sketch
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
	   :define-sketch-setup

	   ::width
	   ::height
	   ::framerate
	   ::title

	   ;; Math
	   :clamp-1
	   :normalize

	   :+two-pi+
	   :+tau+
	   :+half-pi+
	   :+quarter-pi+

	   :radians
	   :degrees

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
	   :color-rgba
	   :color-hsba
	   :lerp-color
	   :random-color
	   :hash-color

	   ;; Pen
	   :pen
	   :make-pen
	   :set-pen
	   :with-pen
	   :background

	   ;; Shapes
	   :ellipse
	   :line
	   :point
	   :rect
	   :ngon
	   :triangle

	   ;; Transforms
	   :ntranslate
	   :nrotate
	   :nscale
	   :with-identity-matrix))


