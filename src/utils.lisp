;;;; utils.lisp

(in-package #:sketch)

;;;  _   _ _____ ___ _     ____
;;; | | | |_   _|_ _| |   / ___|
;;; | | | | | |  | || |   \___ \
;;; | |_| | | |  | || |___ ___) |
;;;  \___/  |_| |___|_____|____/

(defparameter *build* nil)

(defmacro define-start-function ((name &optional toplevel-name)
                                 sketch-name initargs
                                 &rest options)
  "If toplevel-name is not specified uses `<name>-toplevel'.
Possible options:
  :setup - defines `sketch:setup' `:before' method
      (:setup (<arg-name>)
        <body>)
  :on-close - defines `kit.sdl2:on-close' `:before' method;
      (:on-close (<arg-name>)
        <body>)
  :start - executed before creating an instance of sketch (on every function call)
      (:start <body>)
  :quit - executed after the instance is closed (only for toplevel function)
      (:quit <body>)"
  (let ((initargs-name (gensym "INITARGS"))
        (toplevel-name (or toplevel-name
                           (intern (concatenate 'string
                                                (symbol-name name)
                                                "-TOPLEVEL")
                                   (symbol-package name)))))
    (flet ((define-method (name allow-other-keys arg &rest body)
             `(defmethod ,name :before ((,@arg ,sketch-name)
                                        ,@(if allow-other-keys
                                              '(&key &allow-other-keys)))
                (declare (ignorable ,@arg))
                ,@body)))
      `(progn
         ,(alexandria:when-let (arg-and-body (cdr (assoc :setup options)))
            (apply #'define-method 'sketch:setup t arg-and-body))
         ,(alexandria:when-let (arg-and-body (cdr (assoc :on-close options)))
            (apply #'define-method 'kit.sdl2:close-window nil arg-and-body))
         (defun ,name (&rest ,initargs-name &key &allow-other-keys)
           (initialize-sketch)
           ,@(cdr (assoc :start options))
           (apply #'make-instance ',sketch-name (append ,initargs-name ',initargs)))
         (defun ,toplevel-name ()
           (sdl2:make-this-thread-main
            (lambda ()
              (let ((*build* t))
                (initialize-sketch)
                ,@(cdr (assoc :start options))
                (make-instance ',sketch-name ,@initargs))))
           ,@(cdr (assoc :quit options)))
         (values ,name ,toplevel-name)))))

(defun pad-list (list pad length)
  (let ((pad-length (- length (length list))))
    (if (> pad-length 0)
	(append (make-list pad-length :initial-element pad)
		list)
	list)))

(defun group (list &optional (group-length 2))
  (loop with list = (copy-list list)
        for tail = (nthcdr (1- group-length) list)
        while tail
        collect (shiftf list (cdr tail) nil)))

(defun group-bits (x &optional (bits 8))
  (loop with result = ()
        for pos from 0 below (integer-length x) by bits
        do (push (ldb (byte bits pos) x) result)
        finally (return result)))

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

(defun surface-format (surface)
  (plus-c:c-let ((surface sdl2-ffi:sdl-surface :from surface))
    (surface :format :format)))
