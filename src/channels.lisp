;;;; channels.lisp

(in-package #:sketch)

;;;   ____ _   _    _    _   _ _   _ _____ _     ____
;;;  / ___| | | |  / \  | \ | | \ | | ____| |   / ___|
;;; | |   | |_| | / _ \ |  \| |  \| |  _| | |   \___ \
;;; | |___|  _  |/ ___ \| |\  | |\  | |___| |___ ___) |
;;;  \____|_| |_/_/   \_\_| \_|_| \_|_____|_____|____/

;;; Channel interface

(defparameter *channels* (make-hash-table))

(defun drop-first (&optional a b) (declare (ignore a)) b)

(defun register-input (channel &optional (reducer #'drop-first))
  (let ((channel-reducers (gethash channel *channels*)))
    (unless (assoc reducer channel-reducers)
      (setf (gethash channel *channels*)
	    (cons (cons reducer (funcall reducer))
		  channel-reducers)))))

(defun in (channel &optional (initial nil) (reducer #'drop-first))
  (register-input channel reducer)
  (let ((a (cdr (assoc reducer (gethash channel *channels*)))))
    (or a initial)))

(defun out-1 (channel message)
  (register-input channel #'drop-first)
  (mapcar (lambda (reducer-value-cons)
	    (setf (cdr reducer-value-cons)
		  (funcall (car reducer-value-cons)
			   (cdr reducer-value-cons)
			   message)))
	  (gethash channel *channels*))
  (propagate channel))

(defun out (&rest channel-message)
  (mapcar (lambda (x) (out-1 (first x) (second x)))
	  (group channel-message)))

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
	;; If inputs-and-outputs were assigned as '((in) (out))
	;; this would use the same list, so we need to be explicit
	;; at creating a new list each time this function is called.
	(inputs-and-outputs (list (list 'in) (list 'out)))
	(push-into nil))
    (dolist (token flat-body)
      (alexandria:if-let ((io-cons (assoc push-into inputs-and-outputs)))
	(setf (cdr io-cons) (cons token (cdr io-cons))
	      push-into nil)
	(setf push-into token)))
    inputs-and-outputs))

(defun extract-input-registration (body)
  (mapcar (lambda (in-form) (cons 'register-input (cdr in-form)))
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

(defmacro define-channel-propagation (name &body body)
  (let* ((inputs-and-outputs (find-inputs-and-outputs body))
	 (inputs (cdr (assoc 'in inputs-and-outputs)))
	 (outputs (cdr (assoc 'out inputs-and-outputs)))
	 (input-registrations (extract-input-registration body)))
    (update-propagation-data name inputs outputs)
    `(progn
       ,@input-registrations
       (setf (propagation-function (gethash ',name *propagations*))
	      (lambda () ,@body))
       (mapcar #'propagate ',inputs))))

;;; Utility functions

(defun reset-channel (channel)
  (remhash channel *channels*)
  (remhash channel *channel-propagations*)
  (maphash (lambda (name propagation)
	     (setf (propagation-inputs propagation)
		   (remove-if (lambda (x) (eql x channel))
			      (propagation-inputs propagation))
		   (propagation-outputs propagation)
		   (remove-if (lambda (x) (eql x channel))
			      (propagation-outputs propagation))))
	   *propagations*)
  nil)

(defun reset-all-channels ()
  (setf *channels* (make-hash-table)
	*propagations* (make-hash-table)
	*channel-propagations* (make-hash-table))
  nil)
