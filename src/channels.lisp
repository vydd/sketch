;;;; channels.lisp

(in-package #:sketch)

;;;   ____ _   _    _    _   _ _   _ _____ _     ____
;;;  / ___| | | |  / \  | \ | | \ | | ____| |   / ___|
;;; | |   | |_| | / _ \ |  \| |  \| |  _| | |   \___ \
;;; | |___|  _  |/ ___ \| |\  | |\  | |___| |___ ___) |
;;;  \____|_| |_/_/   \_\_| \_|_| \_|_____|_____|____/

;;; Channel interface

(defparameter *channels* (make-hash-table))

(defun register-input (channel &optional initial (adapter #'identity))
  (unless (assoc adapter (gethash channel *channels*))
    (push (cons adapter initial) (gethash channel *channels*)))
  t)

(defun in (channel &optional initial (adapter #'identity))
  (register-input channel initial adapter)
  (let ((a (cdr (assoc adapter (gethash channel *channels*)))))
    (or a initial)))

(defun out-1 (channel message)
  (register-input channel message #'identity)
  (mapcar (lambda (adapter-value-cons)
            (setf (cdr adapter-value-cons)
                  (funcall (car adapter-value-cons) message)))
          (gethash channel *channels*))
  (propagate channel))

(defun out (&rest channel-message)
  (mapcar (lambda (x) (out-1 (first x) (second x)))
          (group channel-message))
  (values))

;;; Channel propagation

(defstruct propagation
  name
  inputs
  outputs
  function)

(defparameter *propagations* (make-hash-table))
(defparameter *channel-propagations* (make-hash-table))

(defun propagate (channel)
  (mapcar (lambda (p) (funcall (propagation-function p)))
          (gethash channel *channel-propagations*)))

(defun find-inputs-and-outputs (body)
  (let ((flat-body (alexandria:flatten body))
        (inputs-and-outputs (list (list 'in) (list 'out)))
        (push-into nil))
    (dolist (token flat-body)
      (alexandria:if-let ((io-cons (assoc push-into inputs-and-outputs)))
        (progn
          (when (not (member token (cdr io-cons)))
            (setf (cdr io-cons) (cons token (cdr io-cons))))
          (setf push-into nil))
        (setf push-into token)))
    inputs-and-outputs))

(defun extract-input-registration (body)
  (mapcar (lambda (in-form) (cadr in-form))
          (remove-if #'atom (flatten body (lambda (x) (eql (car x) 'in))))))

(defun delete-channel-propagation (channel propagation)
  (setf (gethash channel *channel-propagations*)
        (remove-if (lambda (x) (eql x propagation))
                   (gethash channel *channel-propagations*))))

(defun update-propagation-data (name inputs outputs)
  (let ((propagation (gethash name *propagations*)))
    (if propagation
        (mapcar (lambda (channel)
                  (delete-channel-propagation channel propagation))
                (propagation-inputs propagation))
        (setf propagation (make-propagation :name name)
              (gethash name *propagations*) propagation))
    (setf (propagation-inputs propagation) inputs
          (propagation-outputs propagation) outputs)
    (mapcar (lambda (channel)
              (push propagation (gethash channel *channel-propagations*)))
            inputs)))

(defun %define-channel-observer (name body)
  (let ((name (or name (gensym))))
    (let* ((inputs-and-outputs (find-inputs-and-outputs body))
           (inputs (cdr (assoc 'in inputs-and-outputs)))
           (outputs (cdr (assoc 'out inputs-and-outputs)))
           (input-registrations (extract-input-registration body)))
      (update-propagation-data name inputs outputs)
      (mapcar #'register-input input-registrations)
      (setf (propagation-function (gethash name *propagations*))
            (eval `(lambda () ,@body)))
      (when outputs
        (mapcar #'propagate inputs)))))

(defmacro define-named-channel-observer (name &body body)
  (%define-channel-observer name body)
  nil)

(defmacro define-channel-observer (&body body)
  (%define-channel-observer nil body)
  nil)

;;; Utility functions

(defun reset-channel (channel)
  (remhash channel *channels*)
  (remhash channel *channel-propagations*)
  (maphash (lambda (name propagation)
             (declare (ignore name))
             (setf (propagation-inputs propagation)
                   (remove-if (lambda (x) (eql x channel))
                              (propagation-inputs propagation))
                   (propagation-outputs propagation)
                   (remove-if (lambda (x) (eql x channel))
                              (propagation-outputs propagation))))
           *propagations*)
  (values))

(defun reset-all-channels ()
  (setf *channels* (make-hash-table)
        *propagations* (make-hash-table)
        *channel-propagations* (make-hash-table))
  (values))
