;;;; sketch.asd

(asdf:defsystem #:sketch
  :description "Sketch"
  :author "Danilo Vidovic (vydd)"
  :license "MIT"
  :depends-on (#:alexandria
	       #:cffi
	       #:glkit
	       #:mathkit
	       #:md5
	       #:sb-cga
	       #:sdl2
	       #:sdl2kit
	       #:static-vectors)
  :pathname "src"
  :serial t
  :components ((:file "package")
	       (:file "utils")
	       (:file "environment")
	       (:file "color")
	       (:file "pen")
	       (:file "math")
	       (:file "shaders")
	       (:file "geometry")
	       (:file "drawing")
	       (:file "shapes")
	       (:file "transforms")
               (:file "sketch")))
