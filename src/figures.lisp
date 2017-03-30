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
           (arr (env-vert-array env))
           (stream (env-vert-stream env)))
      (dolist (draw draws)
        (let ((primitive (getf draw :primitive))
              (pointer (getf draw :pointer))
              (len (getf draw :length)))
          (adjust-gpu-array arr len)
          (setf (buffer-stream-length stream) len)
          (with-gpu-array-as-pointer (gpu-arr-ptr arr :access-type :write-only)
            (copy-buffer pointer gpu-arr-ptr (* len +bytes-per-vertex+)))
          (fill-primitive primitive (env-white-pixel-sampler *env*)))))))

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
