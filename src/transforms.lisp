;;;; transforms.lisp

(in-package #:sketch)

;;;  _____ ____      _    _   _ ____  _____ ___  ____  __  __ ____
;;; |_   _|  _ \    / \  | \ | / ___||  ___/ _ \|  _ \|  \/  / ___|
;;;   | | | |_) |  / _ \ |  \| \___ \| |_ | | | | |_) | |\/| \___ \
;;;   | | |  _ <  / ___ \| |\  |___) |  _|| |_| |  _ <| |  | |___) |
;;;   |_| |_| \_\/_/   \_\_| \_|____/|_|   \___/|_| \_\_|  |_|____/

(defmacro define-ntransform (name args multiplicant)
  `(defun ,name ,args
     (let ((transform-matrix ,multiplicant))
       (setf (env-model-matrix *env*)
	     (sb-cga:matrix* transform-matrix (env-model-matrix *env*))))))

(define-ntransform ntranslate (dx dy)
  (sb-cga::translate* (coerce dx 'single-float) (coerce dy 'single-float) 0.0))

(define-ntransform nrotate (angle)
  (sb-cga::rotate* 0.0 0.0 (coerce (radians angle) 'single-float)))

(define-ntransform nscale (sx sy)
  (sb-cga::scale* (coerce sx 'single-float) (coerce sy 'single-float) 0.0))

(defmacro with-matrix (matrix &body body)
  `(alexandria:with-gensyms (previous-matrix)
     (progn
       (setf previous-matrix (env-model-matrix *env*)
	     (env-model-matrix *env*) ,matrix)
       ,@body
       (setf (env-model-matrix *env*) previous-matrix))))

(defmacro with-identity-matrix (&body body)
  `(with-matrix sb-cga::+identity-matrix+
     ,@body))

(defmacro with-current-matrix (&body body)
  `(with-matrix (env-model-matrix *env*)
     ,@body))

(defun set-matrix (matrix)
  (setf (env-model-matrix *env*) matrix))
