;;;; tests/math.lisp

(in-package #:sketch-tests)

(def-suite math-tests
  :description "Tests for math utilities"
  :in sketch-tests)

(in-suite math-tests)

(test radians-conversion
  (is (= 0 (sketch:radians 0)))
  (is (< (abs (- sketch:+pi+ (sketch:radians 180))) 1e-10))
  (is (< (abs (- sketch:+half-pi+ (sketch:radians 90))) 1e-10))
  (is (< (abs (- sketch:+two-pi+ (sketch:radians 360))) 1e-10))
  (is (< (abs (- (- sketch:+pi+) (sketch:radians -180))) 1e-10)))

(test degrees-conversion
  (is (= 0 (sketch:degrees 0)))
  (is (< (abs (- 180 (sketch:degrees sketch:+pi+))) 1e-10))
  (is (< (abs (- 90 (sketch:degrees sketch:+half-pi+))) 1e-10))
  (is (< (abs (- 360 (sketch:degrees sketch:+two-pi+))) 1e-10)))

(test radians-degrees-roundtrip
  (loop for deg in '(0 30 45 60 90 120 180 270 360)
        do (is (< (abs (- deg (sketch:degrees (sketch:radians deg)))) 1e-10))))

(test clamp-1
  (is (= 0.0 (sketch:clamp-1 -1.0)))
  (is (= 0.0 (sketch:clamp-1 0.0)))
  (is (= 0.5 (sketch:clamp-1 0.5)))
  (is (= 1.0 (sketch:clamp-1 1.0)))
  (is (= 1.0 (sketch:clamp-1 2.0))))

(test normalize-basic
  (is (= 0.0 (sketch:normalize 0 0 100)))
  (is (= 0.5 (sketch:normalize 50 0 100)))
  (is (= 1.0 (sketch:normalize 100 0 100))))

(test normalize-clamping
  (is (= 0.0 (sketch:normalize -50 0 100 :clamp t)))
  (is (= 1.0 (sketch:normalize 150 0 100 :clamp t)))
  (is (= -0.5 (sketch:normalize -50 0 100 :clamp nil)))
  (is (= 1.5 (sketch:normalize 150 0 100 :clamp nil))))

(test normalize-output-range
  (is (= 0.0 (sketch:normalize 0 0 100 :out-low 0.0 :out-high 10.0)))
  (is (= 5.0 (sketch:normalize 50 0 100 :out-low 0.0 :out-high 10.0)))
  (is (= 10.0 (sketch:normalize 100 0 100 :out-low 0.0 :out-high 10.0))))

(test constants-exist
  (is (numberp sketch:+pi+))
  (is (numberp sketch:+two-pi+))
  (is (numberp sketch:+tau+))
  (is (numberp sketch:+half-pi+))
  (is (numberp sketch:+quarter-pi+))
  (is (numberp sketch:+phi+))
  (is (numberp sketch:+golden-ratio+))
  (is (numberp sketch:+e+))
  (is (= sketch:+two-pi+ sketch:+tau+))
  (is (= sketch:+phi+ sketch:+golden-ratio+)))
