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
  "Sets environment pen to PEN."
  (setf (env-pen *env*) pen))

(defun flip-pen (pen)
  "Makes a new pen by swapping PEN's fill and stroke colors."
  (make-pen :weight (pen-weight pen)
	    :stroke (pen-fill pen)
	    :fill (pen-stroke pen)))

(defun background (color)
  "Fills the sketch window with COLOR."
  (apply #'gl:clear-color (color-rgba color))
  (gl:clear :color-buffer))

(defparameter *default-pen* (make-pen :weight 1 :fill (gray 1) :stroke (gray 0)))
