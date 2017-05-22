;;;; sketch.asd

(asdf:defsystem #:sketch
  :description "Sketch is a Common Lisp framework for the creation of electronic art, computer graphics, visual design, game making and more. It is inspired by Processing and OpenFrameworks."
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
               #:static-vectors
               #:cepl.sdl2
               #:nineveh
               :temporal-functions)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "monkey-patching")
               (:file "math")
               (:file "utils")
               (:file "channels")
               (:file "shaders")
               (:file "environment")
               (:file "pen")
               (:file "font")
               (:file "geometry")
               (:file "drawing")
               (:file "shapes")
               (:file "transforms")
               (:file "resources")
               (:file "color")
               (:file "sketch")
               (:file "figures")
               (:file "controllers")))
