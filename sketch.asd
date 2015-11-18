;;;; sketch.asd

(asdf:defsystem #:sketch
  :description "Sketch"
  :author "Danilo Vidovic (vydd)"
  :license "MIT"
  :depends-on (#:alexandria
	       #:cffi
	       #:glkit
	       #:md5
	       #:sdl2
	       #:sdl2kit
	       #:static-vectors)
  :pathname "src"
  :serial t
  :components ((:file "package")
	       (:file "environment")
	       (:file "color")
	       (:file "pen")
	       (:file "utils")
	       (:file "math")
	       (:file "shaders")
	       (:file "shapes")
	       (:file "transforms")
	       (:file "profiler")
               (:file "sketch")))

