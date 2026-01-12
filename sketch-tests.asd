;;;; sketch-tests.asd

(asdf:defsystem #:sketch-tests
  :description "Tests for Sketch"
  :author "Danilo Vidovic (vydd)"
  :license "MIT"
  :depends-on (#:sketch
               #:fiveam)
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "math")
               (:file "color")
               (:file "utils")
               (:file "geometry")))
