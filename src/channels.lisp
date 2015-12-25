;;;; channels.lisp

(in-package #:sketch)

;;;   ____ _   _    _    _   _ _   _ _____ _     ____
;;;  / ___| | | |  / \  | \ | | \ | | ____| |   / ___|
;;; | |   | |_| | / _ \ |  \| |  \| |  _| | |   \___ \
;;; | |___|  _  |/ ___ \| |\  | |\  | |___| |___ ___) |
;;;  \____|_| |_/_/   \_\_| \_|_| \_|_____|_____|____/

;;; Channel interface

(defparameter *channels* (make-hash-table))
(defparameter *channel-propagators* (make-hash-table))
(defparameter *channel-propagator-body-hashes* '())

(defun drop-first (&optional a b) (declare (ignore a)) b)

(defun register-input (channel &optional (reducer #'drop-first))
  (let ((channel-reducers (gethash channel *channels*)))
    (unless (assoc reducer channel-reducers)
      (setf (gethash channel *channels*)
	    (cons (cons reducer (funcall reducer))
		  channel-reducers)))))

(defun in (channel &optional (reducer #'drop-first))
  (register-input channel reducer)
  (cdr (assoc reducer (gethash channel *channels*))))

(defun out (channel message)
  (register-input channel #'drop-first)
  (mapcar (lambda (reducer-value-cons)
	    (setf (cdr reducer-value-cons)
		  (funcall (car reducer-value-cons)
			   (cdr reducer-value-cons)
			   message)))
	  (gethash channel *channels*))
  (propagate channel))

;;; Channel propagation

(defun propagate (channel)
  (mapcar #'funcall (gethash channel *channel-propagators*)))

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
  (mapcar
   (lambda (in-form) (cons 'register-in (cdr in-form)))
   (remove-if #'atom (flatten body (lambda (x) (eql (car x) 'in))))))

(defmacro define-channel-propagation (&body body)
  (let* ((body-hash (object-to-keyword-hash body))
	 (inputs-and-outputs (find-inputs-and-outputs body))
	 (inputs (cdr (assoc 'in inputs-and-outputs)))
	 (input-registrations (extract-input-registration body))
	 (channels (append (assoc 'in inputs-and-outputs)
			   (assoc 'out inputs-and-outputs))))
    (unless (member body-hash *channel-propagator-body-hashes*)
      (push body-hash *channel-propagator-body-hashes*)
      `(progn
	 ,@input-registrations
	 ,@(mapcar (lambda (input)
		     `(push (lambda () ,@body)
			    (gethash ,input *channel-propagators*)))
		   inputs)))))

;;; Utility reset functions

(defun reset-all-channels ()
  (setf *channels* (make-hash-table)
	*channel-propagators* (make-hash-table)
	*channel-propagator-body-hashes* '()))

;;; Default Sketch channels

(defmethod kit.sdl2:mousewheel-event ((sketch-window sketch) timestamp x y)
  (out :mouse-wheel (cons x y))
  (out :mouse-wheel-x x)
  (out :mouse-wheel-y y))

(defmethod kit.sdl2:mousemotion-event ((sketch-window sketch)
				       timestamp button-mask x y xrel yrel)
  (out :mouse (cons x y))
  (out :mouse-x x)
  (out :mouse-y y)
  (out :mouse-rel (cons xrel yrel))
  (out :mouse-xrel xrel)
  (out :mouse-yrel yrel))
