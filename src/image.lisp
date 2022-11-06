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

(defun save-png (pathname)
  (let ((width (sketch-width *sketch*))
        (height (sketch-height *sketch*)))
    (flet ((ptr (vec offset)
             (static-vectors:static-vector-pointer vec :offset offset))
           (from (row col width)
             (+ col (* row (* 4 width))))
           (to (row col width height)
             (+ col (* (- height row 1) 4 width))))
      (static-vectors:with-static-vector (buffer (* 4 width height))
        (%gl:read-pixels 0 0 width height :rgba :unsigned-byte (ptr buffer 0))
        (dotimes (row (truncate height 2))
          (dotimes (col (* 4 width))
            (rotatef (cffi:mem-aref (ptr buffer (from row col width)) :uint8)
                     (cffi:mem-aref (ptr buffer (to row col width height)) :uint8))))
        (let ((png (make-instance 'zpng:png
                                  :width width
                                  :height height
                                  :color-type :truecolor-alpha
                                  :image-data buffer)))
          (zpng:write-png png pathname))))))
