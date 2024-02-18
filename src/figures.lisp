;;;; figures.lisp

(in-package #:sketch)

;;;  _____ ___ ____ _   _ ____  _____ ____
;;; |  ___|_ _/ ___| | | |  _ \| ____/ ___|
;;; | |_   | | |  _| | | | |_) |  _| \___ \
;;; |  _|  | | |_| | |_| |  _ <| |___ ___) |
;;; |_|   |___\____|\___/|_| \_\_____|____/

(defclass figure ()
  ((draws :initarg :draws)))

(defmethod draw-figure ((figure figure))
  (symbol-macrolet ((position (env-buffer-position *env*)))
    (with-slots (draws) figure
      (kit.gl.shader:uniform-matrix (env-programs *env*) :model-m 4
                                    (vector (env-model-matrix *env*)))
      (gl:bind-texture :texture-2d (env-white-pixel-texture *env*))
      (dolist (draw draws)
        (let ((primitive (getf draw :primitive))
              (pointer (getf draw :pointer))
              (length (getf draw :length)))
          (when (> (* *bytes-per-vertex* (+ position length)) *buffer-size*)
            (start-draw))
          (let ((buffer-pointer
                 (%gl:map-buffer-range :array-buffer
                                       (* position *bytes-per-vertex*)
                                       (* length *bytes-per-vertex*)
                                       +access-mode+)))
            (copy-buffer pointer buffer-pointer (* length *bytes-per-vertex*))
            (%gl:draw-arrays primitive position length)
            (setf position (+ position length))
            (%gl:unmap-buffer :array-buffer)))))))

(defmacro deffigure (name &optional ((&whole opt &optional w h &rest r) '(nil nil)) &body body)
  (declare (ignore r))
  (unless (and (numberp w) (numberp h))
    (warn "Defining a figure with deffigure without specifying dimensions is deprecated.")
    (setf w nil
          h nil
          body (cons opt body)))
  `(let ((*draw-sequence* nil))
     (let ((*env* (make-fake-env))
           (*draw-mode* :figure))
       (with-pen (make-default-pen)
         ,@body))
     (setf *draw-sequence* (nreverse *draw-sequence*))
     (let ((figure (make-instance 'figure :draws *draw-sequence*)))
       ,(if (numberp w)
            `(defun ,name (x y &optional (w ,w) (h ,h))
               (with-current-matrix
                 (translate x y)
                 (scale (/ w ,w) (/ h ,h))
                 (draw-figure figure)))
            (progn
              `(defun ,name (x y)
                 (with-current-matrix
                   (translate x y)
                   (draw-figure figure))))))))
