;;;; transforms.lisp

(in-package #:sketch)

;;;  _____ ____      _    _   _ ____  _____ ___  ____  __  __ ____
;;; |_   _|  _ \    / \  | \ | / ___||  ___/ _ \|  _ \|  \/  / ___|
;;;   | | | |_) |  / _ \ |  \| \___ \| |_ | | | | |_) | |\/| \___ \
;;;   | | |  _ <  / ___ \| |\  |___) |  _|| |_| |  _ <| |  | |___) |
;;;   |_| |_| \_\/_/   \_\_| \_|____/|_|   \___/|_| \_\_|  |_|____/

(defun set-translation (x y)
  (error "SET-TRANSLATION not implemented"))

(defun set-rotation (angle)
  (error "SET-ROTATION not implemented"))

(defun set-scale (sx sy)
  (error "SET-SCALE not implemented"))

(defmacro with-identity-matrix (&body body)
  `(error "WITH-IDENTITY-MATRIX not implemented"))
