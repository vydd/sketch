;;;; bindings.lisp

(in-package #:sketch)

;;;  ____ ___ _   _ ____ ___ _   _  ____ ____
;;; | __ )_ _| \ | |  _ \_ _| \ | |/ ___/ ___|
;;; |  _ \| ||  \| | | | | ||  \| | |  _\___ \
;;; | |_) | || |\  | |_| | || |\  | |_| |___) |
;;; |____/___|_| \_|____/___|_| \_|\____|____/

(defclass binding ()
  ((name :initarg :name :accessor binding-name)
   (prefix :initarg :prefix :accessor binding-prefix)
   (initform :initarg :initform :accessor binding-initform)
   (defaultp :initarg :defaultp :accessor binding-defaultp)
   (initarg :initarg :initarg :accessor binding-initarg)
   (accessor :initarg :accessor :accessor binding-accessor)
   (channelp :initarg :channelp :accessor binding-channelp)
   (channel-name :initarg :channel-name :accessor binding-channel-name)))

(defun make-binding (name prefix
		     &key
		       (defaultp nil)
		       (initform nil)
		       (initarg (alexandria:make-keyword name))
		       (accessor (alexandria:symbolicate prefix '#:- name))
		       (channel-name nil channel-name-p))
  (make-instance 'binding :name name
                          :prefix prefix
                          :defaultp defaultp
                          :initform initform
                          :initarg initarg
                          :accessor accessor
                          :channel-name channel-name
                          :channelp channel-name-p))

(defun parse-bindings (prefix bindings &optional default-slots)
  (loop for (name value . args) in (alexandria:ensure-list bindings)
        for default-slot-p = (assoc name default-slots)
        ;; If a VALUE is of form (IN CHANNEL-NAME DEFAULT-VALUE) it
        ;; is recognized as a channel. We should pass additional
        ;; :channel-name parameter to MAKE-BINDING and set the VALUE
        ;; to the DEFAULT-VALUE.
        when (and (consp value)
                  (eq 'in (car value)))
          do (setf args (list* :channel-name (second value) args)
                   value (third value))
        collect (apply #'make-binding
		       (list*
			name
			(if default-slot-p 'sketch prefix)
			:initform value
			(if default-slot-p (cdddr default-slot-p) args)))))
