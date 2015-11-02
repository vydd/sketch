;;;; package.lisp

(defpackage #:sketch
  (:use #:cl)
  (:export :sketch
	   :size
	   :title
	   :smooth
	   :no-smooth	   
	   
	   ; Math
	   :clamp-1
	   :normalize
	   :radians
	   :degrees

	   ; Colors
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

	   ; Pen
	   :pen
	   :with-pen
	   :background

      	   ; Shapes
	   :ellipse
	   :line
	   :point
	   :quad
	   :rect
	   :ngon
	   :triangle

	   ; Examples
	   :sketch-example-1
	   :sketch-example-2
	   :sketch-example-3
	   ))

