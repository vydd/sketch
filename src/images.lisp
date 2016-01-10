;;;; images.lisp

(in-package #:sketch)

;;;  ___ __  __    _    ____ _____ ____
;;; |_ _|  \/  |  / \  / ___| ____/ ___|
;;;  | || |\/| | / _ \| |  _|  _| \___ \
;;;  | || |  | |/ ___ \ |_| | |___ ___) |
;;; |___|_|  |_/_/   \_\____|_____|____/

(defun white-pixel-texture ()
  "Sent to shaders when no image is active."
  (let ((texture (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-image-2d :texture-2d 0 :rgba 1 1 0 :bgra :unsigned-byte #(255 255 255 255))
    texture))

(defmethod image ((source string) &key (reload nil))

  )
