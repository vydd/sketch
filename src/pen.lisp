;;;; pen.lisp

(in-package #:sketch)

;;;  ____  _____ _   _
;;; |  _ \| ____| \ | |
;;; | |_) |  _| |  \| |
;;; |  __/| |___| |\  |
;;; |_|   |_____|_| \_|

(defstruct pen
  (fill nil)
  (stroke nil)
  (weight 1)
  (curve-steps 100)
  (winding-rule :nonzero
   :type (member :odd :nonzero :positive :negative :abs-geq-two)))

(defun make-default-pen ()
  (make-pen :weight 1 :fill +white+ :stroke +black+))

(define-environment-property :pen
  (make-default-pen))

(defmacro with-pen (pen &body body)
  (with-shorthand (pen make-pen)
    (alexandria:with-gensyms (previous-pen)
      `(let ((,previous-pen (env-pen *env*)))
         (unwind-protect (progn
                           (setf (env-pen *env*) ,pen)
                           ,@body)
           (setf (env-pen *env*) ,previous-pen))))))

(defun set-pen (pen)
  "Sets environment pen to PEN."
  (setf (env-pen *env*) pen))

(defun flip-pen (pen)
  "Makes a new pen by swapping PEN's fill and stroke colors."
  (make-pen :weight (pen-weight pen)
            :stroke (pen-fill pen)
            :fill (pen-stroke pen)
            :weight (pen-weight pen)
            :curve-steps (pen-curve-steps pen)
            :winding-rule (pen-winding-rule pen)))

(defun background (color)
  "Fills the sketch window with COLOR."
  (apply #'gl:clear-color (color-rgba color))
  (gl:clear :color-buffer))
