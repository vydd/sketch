;;;; figures.lisp

(in-package #:sketch)

;;;  _____ ___ ____ _   _ ____  _____ ____
;;; |  ___|_ _/ ___| | | |  _ \| ____/ ___|
;;; | |_   | | |  _| | | | |_) |  _| \___ \
;;; |  _|  | | |_| | |_| |  _ <| |___ ___) |
;;; |_|   |___\____|\___/|_| \_\_____|____/

(defclass figure ()
  ((draws :initarg :draws)))

(defmethod draw ((figure figure) &key &allow-other-keys)
  (with-slots (draws) figure
    (let* ((env *env*)
           (scratch-arr (env-vert-scratch-array env))
           (streamer (env-vert-streamer env)))
      (dolist (draw draws)
        (let ((primitive (getf draw :primitive))
              (pointer (getf draw :pointer))
              (len (getf draw :length)))
          (let ((ptr (c-array-pointer scratch-arr)))
            (copy-buffer pointer ptr (* len +bytes-per-vertex+)))
          (nineveh.streams:buffer-streamer-push-from-range
           scratch-arr streamer 0 len primitive)
          (fill-primitive (env-white-pixel-sampler *env*)))))))

(defmacro deffigure (name &body body)
  `(let ((*draw-sequence* nil))
     (let ((*env* (make-env))
           (*draw-mode* :figure))
       (with-pen (make-default-pen)
         ,@body))
     (setf *draw-sequence* (nreverse *draw-sequence*))
     (let ((figure (make-instance 'figure :draws *draw-sequence*)))
       (defun ,name (x y)
         (translate x y)
         (draw figure)
         (translate (- x) (- y))))))
