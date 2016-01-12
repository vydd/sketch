;;;; sketch.asd

(asdf:defsystem #:sketch
  :description "Sketch"
  :author "Danilo Vidovic (vydd)"
  :license "MIT"
  :depends-on (#:alexandria
	       #:cl-geometry
	       #:glkit
	       #:mathkit
	       #:md5
	       #:sb-cga
	       #:sdl2-image
	       #:sdl2-ttf
	       #:sdl2kit
	       #:static-vectors)
  :pathname "src"
  :serial t
  :components ((:file "package")
	       (:file "math")
	       (:file "utils")
	       (:file "channels")
	       (:file "shaders")
	       (:file "environment")
	       (:file "pen")
	       (:file "geometry")
	       (:file "drawing")
	       (:file "shapes")
	       (:file "transforms")
	       (:file "figures")
	       (:file "resources")
	       (:file "color")
               (:file "sketch")
	       (:file "controllers")))
