;;;; bindings.lisp

(in-package #:sketch)

;;;  ____ ___ _   _ ____ ___ _   _  ____ ____
;;; | __ )_ _| \ | |  _ \_ _| \ | |/ ___/ ___|
;;; |  _ \| ||  \| | | | | ||  \| | |  _\___ \
;;; | |_) | || |\  | |_| | || |\  | |_| |___) |
;;; |____/___|_| \_|____/___|_| \_|\____|____/

;;; TODO:
;;; - Replace defaultp naming (hopefully the logic as well)
;;;   with something less brittle.


(defclass binding ()
  ((name :initarg :name :accessor binding-name)
   (prefix :initarg :prefix :accessor binding-prefix)
   (package :intiarg :package :accessor binding-package)
   (initform :initarg :initform :accessor binding-initform)
   (defaultp :initarg :defaultp :accessor binding-defaultp)
   (initarg :initarg :initarg :accessor binding-initarg)
   (accessor :initarg :accessor :accessor binding-accessor)
   (channelp :initarg :channelp :accessor binding-channelp)
   (channel-name :initarg :channel-name :accessor binding-channel-name)))

(defun make-binding (name prefix
                     &key
                       (package nil)
                       (initform nil)
                       (defaultp nil)
                       (initarg (alexandria:make-keyword name))
                       (accessor (make-accessor name prefix package))
                       (channel-name nil)
                       (channelp (and channel-name t)))
  (make-instance 'binding :name name
                          :prefix prefix
                          :defaultp defaultp
                          :initform initform
                          :initarg initarg
                          :accessor accessor
                          :channel-name channel-name
                          :channelp channelp))

(defun make-accessor (name prefix package)
  (let ((symbol (alexandria:symbolicate prefix '#:- name)))
    (if package
        (intern (symbol-name symbol) (symbol-package prefix))
        symbol)))

(defun copy-binding (binding
                     &key
                       (name (binding-name binding))
                       (prefix (binding-prefix binding))
                       (initform (binding-initform binding))
                       (defaultp (binding-defaultp binding))
                       (initarg (binding-initarg binding))
                       (accessor (binding-accessor binding))
                       (channel-name (binding-channel-name binding))
                       (channelp (and channel-name t)))
  (make-instance 'binding
                 :name name :prefix prefix :initform initform
                 :defaultp defaultp :initarg initarg :accessor accessor
                 :channelp channelp :channel-name channel-name))

(defun class-bindings (class &optional (mark-default-p t))
  (loop for slot in (closer-mop:class-direct-slots class)
        for name = (closer-mop:slot-definition-name slot)
        for initform = (closer-mop:slot-definition-initform slot)
        unless (char= #\% (char (symbol-name name) 0))
          collect (make-binding
                   name
                   (class-name class)
                   :package (symbol-package (class-name class))
                   :defaultp mark-default-p
                   :initform initform)))

(defun parse-bindings (prefix binding-forms &optional existing-bindings)
  (loop for (name value . args) in (alexandria:ensure-list binding-forms)
        for channel-name = (when (channel-value-p value) (second value))
        for existing = (car (member name existing-bindings :key #'binding-name))
        when channel-name
          do (setf value (third value)) ; default channel value
        if existing
          collect (copy-binding
                   existing
                   :initform value
                   :channel-name channel-name
                   :defaultp nil)
            into created
            and collect existing into overriden
        else
          collect (apply #'make-binding (list* name prefix
                                               :initform value
                                               :channel-name channel-name
                                               args))
            into created
        finally
           (let ((remaining-existing
                   (remove-if (lambda (b) (member b overriden))
                              existing-bindings)))
             (return (append remaining-existing created)))))

(defun channel-value-p (value)
  "If a VALUE is of form (IN CHANNEL-NAME DEFAULT-VALUE)
it is recognized as a channel."
  (and (consp value) (eq 'in (car value))))
