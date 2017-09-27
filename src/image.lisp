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
