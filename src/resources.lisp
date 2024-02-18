;;;; resources.lisp

(in-package #:sketch)

;;;  ____  _____ ____   ___  _   _ ____   ____ _____ ____
;;; |  _ \| ____/ ___| / _ \| | | |  _ \ / ___| ____/ ___|
;;; | |_) |  _| \___ \| | | | | | | |_) | |   |  _| \___ \
;;; |  _ <| |___ ___) | |_| | |_| |  _ <| |___| |___ ___) |
;;; |_| \_\_____|____/ \___/ \___/|_| \_\\____|_____|____/

(add-to-environment :resources (make-hash-table))

;;; Classes

(defclass resource () ())

(defclass image (resource)
  ((texture :accessor image-texture :initarg :texture)
   (width :accessor image-width :initarg :width)
   (height :accessor image-height :initarg :height)))

(defclass cropped-image (image)
  ((uv-rect :accessor cropped-image-uv-rect :initarg :uv-rect)
   (original-image :accessor original-image :initarg :original-image)))

(defmethod image-texture ((instance cropped-image))
  (image-texture (original-image instance)))

(defun pixel-uv-rect (img x y w h)
  "Generate uv coordinates (0.0 to 1.0) for portion of IMG within
   the rect specified by X Y W H
   Image flipping can be done by using negative width and height values"
  (with-slots (width height) img
    (list (coerce-float (/ x width))
          (coerce-float (/ y height))
          (coerce-float (/ w width))
          (coerce-float (/ h height)))))

(defun cropped-image-from-image (image x y w h)
  (make-instance 'cropped-image
                 :texture nil
                 :width w
                 :height h
                 :uv-rect (pixel-uv-rect image x y w h)
                 :original-image image))

(defclass typeface (resource)
  ((filename :accessor typeface-filename :initarg :filename)
   (pointer :accessor typeface-pointer :initarg :pointer)))
