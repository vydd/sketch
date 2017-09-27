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
               #:sdl2-image
               #:sdl2-ttf
               #:sdl2kit
               #:split-sequence
               #:static-vectors)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "math")
               (:file "utils")
               (:file "environment")
               (:file "resources")
               (:file "color")
               (:file "channels")
               (:file "shaders")
               (:file "pen")
               (:file "image")
               (:file "font")
               (:file "geometry")
               (:file "drawing")
               (:file "shapes")
               (:file "transforms")
               (:file "sketch")
               (:file "figures")
               (:file "controllers")))
