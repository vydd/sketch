;;;; pen.lisp

(in-package #:sketch)

;;;  ____  _____ _   _
;;; |  _ \| ____| \ | |
;;; | |_) |  _| |  \| |
;;; |  __/| |___| |\  |
;;; |_|   |_____|_| \_|

(defstruct pen
  fill
  stroke
  weight)

(defmacro with-pen (pen &body body)
  (alexandria:once-only (pen)
    `(alexandria:with-gensyms (previous-pen)
       (progn
	 (setf previous-pen (env-pen *env*)
	       (env-pen *env*) ,pen)
	 ,@body
	 (setf (env-pen *env*) previous-pen)))))

(defun set-pen (pen)
  (setf (env-pen *env*) pen))

(defun background (color)
  (apply #'gl:clear-color (color-rgba color))
  (gl:clear :color-buffer))

(defparameter *default-pen* (make-pen :fill (gray 1) :stroke (gray 0)))
