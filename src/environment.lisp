;;;; environment.lisp

(in-package #:sketch)

;;;  _____ _   ___     _____ ____   ___  _   _ __  __ _____ _   _ _____
;;; | ____| \ | \ \   / /_ _|  _ \ / _ \| \ | |  \/  | ____| \ | |_   _|
;;; |  _| |  \| |\ \ / / | || |_) | | | |  \| | |\/| |  _| |  \| | | |
;;; | |___| |\  | \ V /  | ||  _ <| |_| | |\  | |  | | |___| |\  | | |
;;; |_____|_| \_|  \_/  |___|_| \_\\___/|_| \_|_|  |_|_____|_| \_| |_|
;;;
;;; ENVIRONMENT is a contextual store of sketch properties, such as PENs and FONTs.
;;; TODO: Say more.

(defparameter *environment-initializers* (make-hash-table :size 255))

(defun add-environment-initializer (name value)
  (setf (gethash name *environment-initializers*) value))

(defclass environment ()
  ((%properties :initform (make-hash-table :size 255) :accessor environment-%properties)))

(defmethod set-environment-property ((environment environment) property value)
  (setf (gethash property (environment-%properties environment)) value))

(defmethod get-environment-property ((environment environment) property)
  (gethash property (environment-%properties environment)))

(defun make-env ()
  (let ((env (make-instance 'environment)))
    (loop for property being the hash-key
            using (hash-value initializer) of *environment-initializers*
          do (set-environment-property env property (funcall initializer)))
    env))

;;; TODO: Remove. Temporary, for the backend refactor.
(defun make-fake-env ()
  (let ((env (make-instance 'environment)))
    (loop for property being the hash-key of *environment-initializers*
          do (set-environment-property env property nil))
    env))

(defmacro add-to-environment (name &body initializer)
  (let ((fname (alexandria:symbolicate 'env- name))
        (setter-fname (alexandria:symbolicate 'set-env- name) ;(alexandria:make-gensym name)
                      ))
    `(eval-when (:load-toplevel)
       (add-environment-initializer ,name (lambda () ,@initializer))
       (defun ,fname (environment)
         (get-environment-property environment ,name))
       (defun ,setter-fname (environment value)
         (set-environment-property environment ,name value))
       (defsetf ,fname ,setter-fname))))

(defparameter *env* nil)

(defmacro with-environment (env &body body)
  `(let ((*env* ,env))
     ,@body))
