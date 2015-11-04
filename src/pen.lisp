;;;; pen.lisp

(in-package #:sketch)

;;;  ____  _____ _   _
;;; |  _ \| ____| \ | |
;;; | |_) |  _| |  \| |
;;; |  __/| |___| |\  |
;;; |_|   |_____|_| \_|

(defstruct pen
  fill
  stroke)

(defmacro with-pen (pen &body body)
  (alexandria:once-only (pen)
    `(alexandria:with-gensyms (fill stroke)
       (progn
	 (setf fill (env-fill *env*)
	       stroke (env-stroke *env*)
	       (env-fill *env*) (pen-fill ,pen)
	       (env-stroke *env*) (pen-stroke ,pen))
	 ,@body
	 (setf (env-fill *env*) fill
	       (env-stroke *env*) stroke)))))

(defun fill-color (color)
  (setf (env-fill *env*) color))

(defun no-fill-color ()
  (setf (env-fill *env*) nil))

(defun stroke-color (color)
  (setf (env-stroke *env*) color))

(defun no-stroke-color ()
  (setf (env-stroke *env*) nil))

(defun background (color)
  (apply #'gl:clear-color (color-rgba color))
  (gl:clear :color-buffer-bit))
