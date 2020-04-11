;;;; resources.lisp

(in-package #:sketch)

;;  ___ __  __    _    ____ _____ ____
;; |_ _|  \/  |  / \  / ___| ____/ ___|
;;  | || |\/| | / _ \| |  _|  _| \___ \
;;  | || |  | |/ ___ \ |_| | |___ ___) |
;; |___|_|  |_/_/   \_\____|_____|____/

(defun image (image-resource x y &optional width height)
  (with-pen (make-pen :fill image-resource
                      :stroke (pen-stroke (env-pen *env*))
                      :weight (pen-weight (env-pen *env*)))
    (rect x
          y
          (or (abs-or-rel width (image-width image-resource)))
          (or (abs-or-rel height (image-height image-resource))))))

(defun pixel-uv-rect (img pixel-rect)
  "Generate uv coordinates (0.0 to 1.0) for portion of IMG within PIXEL-RECT
   PIXEL-RECT is a list of (x y w h).
   Image flipping can be done by using negative width and height values"
  (with-slots (width height) img
    (destructuring-bind (x y w h) pixel-rect
      (list (coerce-float (/ x width))
            (coerce-float (/ y height))
            (coerce-float (/ w width))
            (coerce-float (/ h height))))))

(defun image-portion (image-resource dest-rect src-rect &key stroke weight)
  "Draw a portion of IMAGE-RESOURCE at DEST-RECT, with the portion defined by SRC-RECT.
   DEST-RECT is the rectangle sent to the RECT function, and should be (x y w h)
   SRC-RECT is the rectangle (in pixels) within the image, and should be (x y w h) as well
   :STROKE and :WEIGHT are used if you want to draw a border around the drawn image"
  (with-pen (make-pen :fill image-resource
                      :stroke stroke
                      :weight weight)
      (with-uv-rect (pixel-uv-rect image-resource src-rect)
        (apply #'rect dest-rect))))
