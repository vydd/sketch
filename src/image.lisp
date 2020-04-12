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

(defmethod crop ((image-resource image) x y w h)
  "Generate a cropped image resource from IMAGE-RESOURCE, limiting how much of the image is drawn
   to the rect of X,Y,W,H, which are all in pixel values."
  (cropped-image-from-image image-resource x y w h))

