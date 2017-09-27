;;;; utils.lisp

(in-package #:sketch)

;;;  _   _ _____ ___ _     ____
;;; | | | |_   _|_ _| |   / ___|
;;; | | | | | |  | || |   \___ \
;;; | |_| | | |  | || |___ ___) |
;;;  \___/  |_| |___|_____|____/

(defparameter *build* nil)

(defun pad-list (list pad length)
  (if (>= (length list) length)
      list
      (append (make-list (- length (length list)) :initial-element pad)
              list)))

(defun group (list &optional (group-length 2))
  (flet ((split-n (list n)
           (when (>= (length list) n)
             (loop with acc = '()
                for i below n
                do (setf acc (cons (car list) acc)
                         list (cdr list))
                finally (return (cons (nreverse acc) list))))))
    (loop with acc = '()
       while (or (not acc) (cdr list))
       do (let ((split (split-n list group-length)))
            (when (car split)
              (setf acc (cons (car split) acc)))
            (setf list (cdr split)))
       finally (return (nreverse acc)))))

(defun group-bits (x &optional (bits 8))
  (let ((bit-fill (1- (expt 2 bits))))
    (do* ((x x (ash x (- bits)))
          (acc `(,(boole boole-and x bit-fill))
               (cons (boole boole-and x bit-fill) acc)))
         ((zerop x) (cdr acc)))))

(declaim (inline order-list))
(defun order-list (order list)
  (loop for o in order
     collect (nth o list)))

(declaim (inline mix-lists))
(defun mix-lists (&rest lists)
  (apply #'append (apply #'mapcar #'list lists)))

(declaim (inline div2-inexact))
(defun div2-inexact (a)
  (multiple-value-bind (x y)
      (floor a 2)
    (values x (+ x y))))

(defun abs-or-rel (val src)
  (if (numberp val)
      (cond ((< 0 val 1) (* src val))
            ((<= 1 val) val)
            (t src))
      (or src 0)))

(declaim (inline lerp-list))
(defun lerp-lists (v list-a list-b)
  (mapcar (lambda (a b) (alexandria:lerp v a b)) list-a list-b))

(defun flatten (tree &optional (unless-test (lambda (_) (declare (ignore _)) nil)))
  (let (list)
    (labels ((traverse (subtree)
               (when subtree
                 (if (and (consp subtree) (not (funcall unless-test subtree)))
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree list)))))
      (traverse tree))
    (nreverse list)))

(defun object-to-keyword-hash (object)
  "Expensive operation that turns CL objects into keywords whose names
are MD5 hashes of those objects, stringified. Uniqueness is not guaranteed,
but may be considered unique for all practical purposes."
  (alexandria:make-keyword
   (apply #'alexandria:symbolicate
          (coerce (map 'array (lambda (x) (format nil "~x" x))
                       (md5:md5sum-string (write-to-string object)))
                  'list))))

(defun coerce-float (x)
  (coerce x 'single-float))

(defun copy-buffer (src dst length &key (src-offset 0) (dst-offset 0))
  (declare (optimize (speed 3) (debug 0)))
  (loop for i from 0 below length
     do (setf (cffi:mem-aref dst :uint8 (+ i src-offset))
              (cffi:mem-aref src :uint8 (+ i dst-offset)))))

(defun relative-path (path &optional (system 'sketch))
  (if *build*
      path
      (format nil "~a" (asdf:system-relative-pathname system path))))
