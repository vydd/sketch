;;;; sketch.asd

(asdf:defsystem #:sketch
  :description "Sketch"
  :author "Danilo Vidovic (vydd)"
  :license "MIT"
  :depends-on (#:alexandria
	       #:glkit
	       #:mathkit
	       #:md5
	       #:sb-cga
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
	       (:file "shapes")
	       (:file "transforms")
	       (:file "profiler")
               (:file "sketch")))

