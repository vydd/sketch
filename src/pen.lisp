;;;; pen.lisp

(in-package #:sketch)

;;;  ____  _____ _   _
;;; |  _ \| ____| \ | |
;;; | |_) |  _| |  \| |
;;; |  __/| |___| |\  |
;;; |_|   |_____|_| \_|

(add-to-environment :pen (make-default-pen))

(defstruct pen
  (fill nil)
  (stroke nil)
  (weight 1)
  (curve-steps 100)
  (winding-rule :nonzero
   :type (member :odd :nonzero :positive :negative :abs-geq-two)))

(let ((pen))
  (defun make-default-pen ()
    (setf pen (or pen (make-pen :weight 1 :fill +white+ :stroke +black+)))))

(defun set-pen (pen)
  (setf (env-pen *env*) pen))

(defun flip-pen (pen)
  "Makes a new pen by swapping PEN's fill and stroke colors."
  (make-pen :weight (pen-weight pen)
            :stroke (pen-fill pen)
            :fill (pen-stroke pen)
            :weight (pen-weight pen)
            :curve-steps (pen-curve-steps pen)
            :winding-rule (pen-winding-rule pen)))

(defmacro with-pen (pen &body body)
  (with-shorthand (pen make-pen)
    (alexandria:with-gensyms (previous-pen)
      `(let ((,previous-pen (env-pen *env*)))
         (unwind-protect (progn
                           (setf (env-pen *env*) ,pen)
                           ,@body)
           (setf (env-pen *env*) ,previous-pen))))))
