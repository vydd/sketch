;;;; sketch.asd

(asdf:defsystem #:sketch
  :description "Sketch is a Common Lisp framework for the creation of electronic art, computer graphics, visual design, game making and more. It is inspired by Processing and OpenFrameworks."
  :author "Danilo Vidovic (vydd)"
  :license "MIT"
  :depends-on (#:alexandria
               #:closer-mop
               #:glkit
               #:glu-tessellate
               #:mathkit
               #:md5
               #:sdl2
               #:cl-plus-c
               #:sdl2-image
               #:sdl2-ttf
               #:sdl2kit
               #:split-sequence
               #:static-vectors
               #:trivial-garbage
               #:zpng)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "math")
               (:file "utils")
               (:file "environment")
               (:file "resources")
               (:file "color")
               (:file "channels")
               (:file "pen")
               (:file "geometry")
               (:file "image")
               (:file "shapes")
               (:file "transforms")
               (:file "bindings")
               ;; Backend-dependent
               (:file "resource-loading")
               (:file "font")
               (:file "shaders")
               (:file "drawing")
               (:file "sketch")
               (:module "backend"
                        :depends-on ("package")
                        :serial t
                        :components ((:file "window")
                                     (:file "sdl2backend")))
               (:file "entities") ; depends on sketch
               (:file "figures")
               (:file "controllers")
               (:file "canvas")))
