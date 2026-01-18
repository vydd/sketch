;;;; tests/color.lisp

(in-package #:sketch-tests)

(def-suite color-tests
  :description "Tests for color utilities"
  :in sketch-tests)

(in-suite color-tests)

(defun approx= (a b &optional (tolerance 1e-6))
  (< (abs (- a b)) tolerance))

;;; RGB to HSB conversion tests

(test rgb-to-hsb-red
  (let ((hsb (sketch::rgb-to-hsb 1.0 0.0 0.0)))
    (is (approx= 0.0 (first hsb)))      ; hue
    (is (approx= 1.0 (second hsb)))     ; saturation
    (is (approx= 1.0 (third hsb)))))    ; brightness

(test rgb-to-hsb-green
  (let ((hsb (sketch::rgb-to-hsb 0.0 1.0 0.0)))
    (is (approx= (/ 120 360) (first hsb)))  ; hue = 1/3
    (is (approx= 1.0 (second hsb)))
    (is (approx= 1.0 (third hsb)))))

(test rgb-to-hsb-blue
  (let ((hsb (sketch::rgb-to-hsb 0.0 0.0 1.0)))
    (is (approx= (/ 240 360) (first hsb)))  ; hue = 2/3
    (is (approx= 1.0 (second hsb)))
    (is (approx= 1.0 (third hsb)))))

(test rgb-to-hsb-white
  (let ((hsb (sketch::rgb-to-hsb 1.0 1.0 1.0)))
    (is (approx= 0.0 (second hsb)))     ; saturation = 0
    (is (approx= 1.0 (third hsb)))))    ; brightness = 1

(test rgb-to-hsb-black
  (let ((hsb (sketch::rgb-to-hsb 0.0 0.0 0.0)))
    (is (approx= 0.0 (second hsb)))     ; saturation = 0
    (is (approx= 0.0 (third hsb)))))    ; brightness = 0

(test rgb-to-hsb-gray
  (let ((hsb (sketch::rgb-to-hsb 0.5 0.5 0.5)))
    (is (approx= 0.0 (second hsb)))     ; saturation = 0
    (is (approx= 0.5 (third hsb)))))    ; brightness = 0.5

;;; HSB to RGB conversion tests

(test hsb-to-rgb-red
  (let ((rgb (sketch::hsb-to-rgb 0.0 1.0 1.0)))
    (is (approx= 1.0 (first rgb)))
    (is (approx= 0.0 (second rgb)))
    (is (approx= 0.0 (third rgb)))))

(test hsb-to-rgb-green
  (let ((rgb (sketch::hsb-to-rgb (/ 120 360) 1.0 1.0)))
    (is (approx= 0.0 (first rgb)))
    (is (approx= 1.0 (second rgb)))
    (is (approx= 0.0 (third rgb)))))

(test hsb-to-rgb-blue
  (let ((rgb (sketch::hsb-to-rgb (/ 240 360) 1.0 1.0)))
    (is (approx= 0.0 (first rgb)))
    (is (approx= 0.0 (second rgb)))
    (is (approx= 1.0 (third rgb)))))

(test hsb-to-rgb-white
  (let ((rgb (sketch::hsb-to-rgb 0.0 0.0 1.0)))
    (is (approx= 1.0 (first rgb)))
    (is (approx= 1.0 (second rgb)))
    (is (approx= 1.0 (third rgb)))))

(test hsb-to-rgb-black
  (let ((rgb (sketch::hsb-to-rgb 0.0 0.0 0.0)))
    (is (approx= 0.0 (first rgb)))
    (is (approx= 0.0 (second rgb)))
    (is (approx= 0.0 (third rgb)))))

;;; Roundtrip tests

(test rgb-hsb-roundtrip
  (loop for (r g b) in '((1.0 0.0 0.0)
                         (0.0 1.0 0.0)
                         (0.0 0.0 1.0)
                         (1.0 1.0 0.0)
                         (0.0 1.0 1.0)
                         (1.0 0.0 1.0)
                         (0.5 0.5 0.5)
                         (0.2 0.4 0.6))
        do (let* ((hsb (sketch::rgb-to-hsb r g b))
                  (rgb (sketch::hsb-to-rgb (first hsb) (second hsb) (third hsb))))
             (is (approx= r (first rgb) 1e-5))
             (is (approx= g (second rgb) 1e-5))
             (is (approx= b (third rgb) 1e-5)))))

;;; Color constructor tests

(test rgb-constructor
  (let ((c (sketch:rgb 0.5 0.3 0.1)))
    (is (approx= 0.5 (sketch:color-red c)))
    (is (approx= 0.3 (sketch:color-green c)))
    (is (approx= 0.1 (sketch:color-blue c)))
    (is (approx= 1.0 (sketch:color-alpha c)))))

(test rgb-constructor-with-alpha
  (let ((c (sketch:rgb 0.5 0.3 0.1 0.8)))
    (is (approx= 0.8 (sketch:color-alpha c)))))

(test rgb-constructor-clamping
  (let ((c (sketch:rgb 1.5 -0.5 0.5)))
    (is (approx= 1.0 (sketch:color-red c)))
    (is (approx= 0.0 (sketch:color-green c)))
    (is (approx= 0.5 (sketch:color-blue c)))))

(test gray-constructor
  (let ((c (sketch:gray 0.5)))
    (is (approx= 0.5 (sketch:color-red c)))
    (is (approx= 0.5 (sketch:color-green c)))
    (is (approx= 0.5 (sketch:color-blue c)))))

(test rgb-255-constructor
  (let ((c (sketch:rgb-255 255 128 0)))
    (is (approx= 1.0 (sketch:color-red c)))
    (is (approx= (/ 128 255) (sketch:color-green c)))
    (is (approx= 0.0 (sketch:color-blue c)))))

;;; Hex color parsing tests

(test hex-to-color-6-digit
  (let ((c (sketch:hex-to-color "ff0000")))
    (is (approx= 1.0 (sketch:color-red c)))
    (is (approx= 0.0 (sketch:color-green c)))
    (is (approx= 0.0 (sketch:color-blue c)))))

(test hex-to-color-6-digit-with-hash
  (let ((c (sketch:hex-to-color "#00ff00")))
    (is (approx= 0.0 (sketch:color-red c)))
    (is (approx= 1.0 (sketch:color-green c)))
    (is (approx= 0.0 (sketch:color-blue c)))))

(test hex-to-color-3-digit
  (let ((c (sketch:hex-to-color "f00")))
    (is (approx= 1.0 (sketch:color-red c)))
    (is (approx= 0.0 (sketch:color-green c)))
    (is (approx= 0.0 (sketch:color-blue c)))))

(test hex-to-color-3-digit-with-hash
  (let ((c (sketch:hex-to-color "#0f0")))
    (is (approx= 0.0 (sketch:color-red c)))
    (is (approx= 1.0 (sketch:color-green c)))
    (is (approx= 0.0 (sketch:color-blue c)))))

(test hex-to-color-8-digit-with-alpha
  (let ((c (sketch:hex-to-color "ff000080")))
    (is (approx= 1.0 (sketch:color-red c)))
    (is (approx= 0.0 (sketch:color-green c)))
    (is (approx= 0.0 (sketch:color-blue c)))
    (is (approx= (/ #x80 255) (sketch:color-alpha c)))))

;;; Color accessor tests

(test color-rgba
  (let* ((c (sketch:rgb 0.1 0.2 0.3 0.4))
         (rgba (sketch:color-rgba c)))
    (is (approx= 0.1 (first rgba)))
    (is (approx= 0.2 (second rgba)))
    (is (approx= 0.3 (third rgba)))
    (is (approx= 0.4 (fourth rgba)))))

(test color-hsba
  (let* ((c (sketch:rgb 1.0 0.0 0.0))
         (hsba (sketch:color-hsba c)))
    (is (approx= 0.0 (first hsba)))     ; hue
    (is (approx= 1.0 (second hsba)))    ; saturation
    (is (approx= 1.0 (third hsba)))     ; brightness
    (is (approx= 1.0 (fourth hsba)))))  ; alpha

;;; Predefined colors

(test predefined-colors-exist
  (is (typep sketch:+red+ 'sketch:color))
  (is (typep sketch:+green+ 'sketch:color))
  (is (typep sketch:+blue+ 'sketch:color))
  (is (typep sketch:+yellow+ 'sketch:color))
  (is (typep sketch:+magenta+ 'sketch:color))
  (is (typep sketch:+cyan+ 'sketch:color))
  (is (typep sketch:+orange+ 'sketch:color))
  (is (typep sketch:+white+ 'sketch:color))
  (is (typep sketch:+black+ 'sketch:color))
  (is (typep sketch:+gray+ 'sketch:color)))

(test predefined-color-values
  (is (approx= 1.0 (sketch:color-red sketch:+red+)))
  (is (approx= 0.0 (sketch:color-green sketch:+red+)))
  (is (approx= 0.0 (sketch:color-blue sketch:+red+)))
  (is (approx= 1.0 (sketch:color-red sketch:+white+)))
  (is (approx= 1.0 (sketch:color-green sketch:+white+)))
  (is (approx= 1.0 (sketch:color-blue sketch:+white+)))
  (is (approx= 0.0 (sketch:color-red sketch:+black+)))
  (is (approx= 0.0 (sketch:color-green sketch:+black+)))
  (is (approx= 0.0 (sketch:color-blue sketch:+black+))))

;;; Color filter tests

(test color-filter-rotate
  (let* ((c (sketch:rgb 0.1 0.2 0.3))
         (rotated (sketch:color-filter-rotate c)))
    (is (approx= 0.2 (sketch:color-red rotated)))
    (is (approx= 0.3 (sketch:color-green rotated)))
    (is (approx= 0.1 (sketch:color-blue rotated)))))

(test color-filter-grayscale-average
  (let* ((c (sketch:rgb 0.3 0.6 0.9))
         (gray (sketch:color-filter-grayscale c :average)))
    (is (approx= 0.6 (sketch:color-red gray)))
    (is (approx= 0.6 (sketch:color-green gray)))
    (is (approx= 0.6 (sketch:color-blue gray)))))
