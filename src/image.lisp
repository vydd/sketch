;;;; resources.lisp

(in-package #:sketch)

;;  ___ __  __    _    ____ _____ ____
;; |_ _|  \/  |  / \  / ___| ____/ ___|
;;  | || |\/| | / _ \| |  _|  _| \___ \
;;  | || |  | |/ ___ \ |_| | |___ ___) |
;; |___|_|  |_/_/   \_\____|_____|____/

(defmethod draw ((image image) &key (x 0) (y 0) width height mode &allow-other-keys)
  "Draws an image, X and Y values are 0 by default, while WIDTH and HEIGHT
are set to the width & height of the image if not provided."
  (declare (ignore mode))
  (with-pen (make-pen :fill image :stroke nil)
    (rect x
          y
          (or (abs-or-rel width (image-width image)))
          (or (abs-or-rel height (image-height image))))))

(defun image (image-resource x y &optional width height)
  "***Deprecated***, use the DRAW method."
  (draw image-resource :x x :y y :width width :height height))

(defmethod crop ((image-resource image) x y w h)
  "Generate a cropped image resource from IMAGE-RESOURCE, limiting how much
of the image is drawn to the rect of X,Y,W,H, which are all in pixel values, and
X & Y are relative to the image."
  (cropped-image-from-image image-resource x y w h))
