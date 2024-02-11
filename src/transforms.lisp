;;;; transforms.lisp

(in-package #:sketch)

;;;  _____ ____      _    _   _ ____  _____ ___  ____  __  __ ____
;;; |_   _|  _ \    / \  | \ | / ___||  ___/ _ \|  _ \|  \/  / ___|
;;;   | | | |_) |  / _ \ |  \| \___ \| |_ | | | | |_) | |\/| \___ \
;;;   | | |  _ <  / ___ \| |\  |___) |  _|| |_| |  _ <| |  | |___) |
;;;   |_| |_| \_\/_/   \_\_| \_|____/|_|   \___/|_| \_\_|  |_|____/

(add-to-environment :model-matrix (sb-cga:identity-matrix))
(add-to-environment :view-matrix nil)
(add-to-environment :matrix-stack nil)

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
  (set-matrix* (sb-cga::scale* (coerce-float sx) (coerce-float (or sy sx)) 1.0))
  (translate (- cx) (- cy)))

(defmacro with-matrix (matrix &body body)
  `(progn
     (push-matrix)
     (set-matrix ,matrix)
     (multiple-value-prog1 (progn ,@body)
       (pop-matrix))))

(defmacro with-identity-matrix (&body body)
  `(with-matrix sb-cga::+identity-matrix+
     ,@body))

(defmacro with-current-matrix (&body body)
  `(with-matrix (env-model-matrix *env*)
     ,@body))

(defmacro with-translate ((dx dy) &body body)
  `(with-current-matrix
     (translate ,dx ,dy)
     ,@body))

(defmacro with-rotate ((angle &optional (cx 0) (cy 0)) &body body)
  `(with-current-matrix
     (rotate ,angle ,cx ,cy)
     ,@body))

(defmacro with-scale ((sx &optional sy (cx 0) (cy 0)) &body body)
  `(with-current-matrix
     (scale ,sx ,sy ,cx ,cy)
     ,@body))

(defun transform-vertex (vertex matrix)
  (let* ((vector (sb-cga:vec
                  (coerce (car vertex) 'single-float)
                  (coerce (cadr vertex) 'single-float)
                  0.0))
         (transformed (sb-cga:transform-point vector matrix)))
    ;; TODO: This is painfully inelegant.
    ;; No consing should happen at this point.
    (list (elt transformed 0) (elt transformed 1))))
