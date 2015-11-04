;;;; sketch.asd

(asdf:defsystem #:sketch
  :description "Sketch"
  :author "Danilo Vidovic (vydd)"
  :license "MIT"
  :depends-on (#:alexandria
	       #:sdl2
	       #:sdl2kit)
  :pathname "src"
  :serial t
  :components ((:file "package")	       
	       (:file "color")
	       (:file "pen")
	       (:file "utils")
	       (:file "math")
	       (:file "shapes")
	       (:file "transforms")	       
               (:file "sketch")))

