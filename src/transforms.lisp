;;;; transforms.lisp

(in-package #:sketch)

;;;  _____ ____      _    _   _ ____  _____ ___  ____  __  __ ____
;;; |_   _|  _ \    / \  | \ | / ___||  ___/ _ \|  _ \|  \/  / ___|
;;;   | | | |_) |  / _ \ |  \| \___ \| |_ | | | | |_) | |\/| \___ \
;;;   | | |  _ <  / ___ \| |\  |___) |  _|| |_| |  _ <| |  | |___) |
;;;   |_| |_| \_\/_/   \_\_| \_|____/|_|   \___/|_| \_\_|  |_|____/

(defun set-matrix (matrix)
  (setf (env-model-matrix *env*) matrix))

(defun push-matrix ()
  (push (env-model-matrix *env*) (env-matrix-stack *env*)))

(defun pop-matrix ()
  (setf (env-model-matrix *env*) (pop (env-matrix-stack *env*))))

(defun set-matrix* (matrix)
  (set-matrix (sb-cga:matrix* (env-model-matrix *env*) matrix)))

(defun translate (dx dy)
  (when (or (not (zerop dx)) (not (zerop dy)))
    (set-matrix* (sb-cga::translate* (coerce-float dx) (coerce-float dy) 0.0))))

(defun rotate (angle &optional (cx 0) (cy 0))
  (translate cx cy)
  (set-matrix* (sb-cga::rotate* 0.0 0.0 (coerce-float (radians angle))))
  (translate (- cx) (- cy)))

(defun scale (sx &optional sy (cx 0) (cy 0))
  (translate cx cy)
  (set-matrix* (sb-cga::scale* (coerce-float sx) (coerce-float (or sy sx)) 0.0))
  (translate (- cx) (- cy)))

(defmacro with-matrix (matrix &body body)
  `(progn
     (push-matrix)
     (set-matrix ,matrix)
     ,@body
     (pop-matrix)))

(defmacro with-identity-matrix (&body body)
  `(with-matrix sb-cga::+identity-matrix+
     ,@body))

(defmacro with-current-matrix (&body body)
  `(with-matrix (env-model-matrix *env*)
     ,@body))
