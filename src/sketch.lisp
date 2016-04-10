;;;; sketch.lisp

(in-package #:sketch)

;;; "sketch" goes here. Hacks and glory await!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;;     _|_|_|  _|    _|  _|_|_|_|  _|_|_|_|_|    _|_|_|  _|    _|   ;;;
;;;   _|        _|  _|    _|            _|      _|        _|    _|   ;;;
;;;     _|_|    _|_|      _|_|_|        _|      _|        _|_|_|_|   ;;;
;;;         _|  _|  _|    _|            _|      _|        _|    _|   ;;;
;;;   _|_|_|    _|    _|  _|_|_|_|      _|        _|_|_|  _|    _|   ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Sketch class

(defclass sketch (kit.sdl2:gl-window)
  (;; Environment
   (%env :initform (make-env))
   (%restart :initform t)
   ;; Window parameters. When modifying, don't forget to
   ;; do the same for #'WINDOW-PARAMETER-BINDINGS.
   (title :initform "Sketch" :reader sketch-title :initarg :sketch-title)
   (width :initform 400 :reader sketch-width :initarg :sketch-width)
   (height :initform 400 :reader sketch-height :initarg :sketch-height)
   (fullscreen :initform nil :reader sketch-fullscreen :initarg :sketch-fullscreen)
   (copy-pixels :initform nil :accessor sketch-copy-pixels :initarg :sketch-copy-pixels)
   (y-axis :initform :down :reader sketch-y-axis :initarg :sketch-y-axis)))

;;; Non trivial sketch writers

(defmacro define-sketch-writer (slot &body body)
  `(defmethod (setf ,(alexandria:symbolicate 'sketch- slot)) (value (instance sketch))
     (setf (slot-value instance ',slot) value)
     (let ((win (kit.sdl2:sdl-window instance)))
       ,@body)))

(define-sketch-writer title
  (sdl2:set-window-title win (slot-value instance 'title)))

(define-sketch-writer width
  (sdl2:set-window-size win (slot-value instance 'width)
			(slot-value instance 'height)))

(define-sketch-writer height
  (sdl2:set-window-size win (slot-value instance 'width)
			(slot-value instance 'height)))

(define-sketch-writer fullscreen
  (sdl2:set-window-fullscreen win (slot-value instance 'fullscreen)))

(define-sketch-writer y-axis
  (with-slots ((env %env) width height y-axis) instance
    (setf (env-view-matrix env)
	  (if (eq y-axis :down)
	      (kit.glm:ortho-matrix 0 width height 0 -1 1)
	      (kit.glm:ortho-matrix 0 width 0 height -1 1)))
    (kit.gl.shader:uniform-matrix
     (env-programs env) :view-m 4 (vector (env-view-matrix env)))))

;;; Generic functions

(defgeneric prepare (instance &key &allow-other-keys)
  (:method-combination progn :most-specific-last))

(defgeneric setup (instance &key &allow-other-keys)
  (:documentation "Called before creating the sketch window.")
  (:method ((instance sketch) &key &allow-other-keys) ()))

(defgeneric draw (instance &key &allow-other-keys)
  (:documentation "Called repeatedly after creating the sketch window,
used for drawing, 60fps.")
  (:method ((instance sketch) &key &allow-other-keys) ()))

;;; Initialization

(defmethod initialize-instance :after ((instance sketch) &rest initargs &key &allow-other-keys)
  (initialize-environment instance)
  (apply #'prepare (list* instance initargs))
  (initialize-gl instance))

;;; Rendering

(defmacro gl-catch (error-color &body body)
  `(handler-case
       (progn
	 ,@body)
     (error (e)
       (progn
	 (background ,error-color)
	 (with-font (make-default-font)
	   (with-identity-matrix
	     (text "ERROR" 20 20)
	     (text (format nil "~a" e) 20 40)
	     (text "Click for restarts." 20 60)))
	 (setf %restart t
	       (env-red-screen *env*) t)))))

(defun draw-window (window)
  (start-draw)
  (draw window)
  (end-draw))

(defmethod kit.sdl2:render ((instance sketch))
  (with-slots (%env width height %restart copy-pixels) instance
    (with-environment %env
      (with-pen (make-default-pen)
	(with-font (make-default-font)
	  (with-identity-matrix
	    (unless copy-pixels
	      (background (gray 0.4)))
	    ;; Restart sketch on setup and when recovering from an error.
	    (when %restart
	      (gl-catch (rgb 1 1 0)
		(setup instance))
	      (setf (slot-value instance '%restart) nil))
	    ;; If we're in the debug mode, we exit from it immediately,
	    ;; so that the restarts are shown only once. Afterwards, we
	    ;; continue presenting the user with the red screen, waiting for
	    ;; the error to be fixed, or for the debug key to be pressed again.
	    (if (debug-mode-p)
		(progn
		  (exit-debug-mode)
		  (draw-window instance))
		(gl-catch (rgb 1 0 0)
		  (draw-window instance)))))))))

;;; Default events

(defmethod close-window :before ((instance sketch))
  (with-environment (slot-value instance '%env)
    (loop for resource being the hash-values of (env-resources *env*)
       do (free-resource resource))))

;;; DEFSKETCH helpers

(defun first-two (list)
  (list (car list) (cadr list)))

(defun default-sketch-slot-p (binding)
  (alexandria:starts-with-subseq
   "sketch-" (symbol-name (car binding)) :test #'string-equal))

(defun default-slots (bindings)
  (remove-if-not #'default-sketch-slot-p bindings))

(defun custom-slots (bindings)
  (remove-if #'default-sketch-slot-p bindings))

(defun binding-accessor (sketch binding)
  (or (cadr (member :accessor (cddr binding)))
      (alexandria:symbolicate sketch '- (car binding))))

(defun make-slot-form (sketch binding)
  (let ((name (car binding))
	(accessor (binding-accessor sketch binding)))
    `(,name :initarg ,(alexandria:make-keyword name)
	    :accessor ,(binding-accessor sketch binding))))

(defun channel-binding-p (binding)
  (and (consp (cadr binding)) (eql 'in (caadr binding))))

(defun make-channel-observer (binding)
  `(define-channel-observer nil
     (let ((win (kit.sdl2:last-window)))
       (when win
	 (setf (slot-value win ',(car binding)) ,(cadr binding))))))

(defun make-channel-observers (bindings)
  (mapcan (lambda (binding)
	    (when (channel-binding-p binding)
	      (make-channel-observer binding)))
       bindings))

(defun replace-channels-with-values (bindings)
  (loop for binding in bindings
     collect (list (car binding)
		   (if (channel-binding-p binding)
		       (caddr (cadr binding))
		       (cadr binding)))))

(defun sketch-bindings-to-slots (sketch bindings)
  (mapcar (lambda (x) (make-slot-form sketch x))
	  (custom-slots bindings)))

(defun window-parameter-bindings (bindings)
  (let ((slots (mapcar #'car (default-slots bindings))))
    (remove-if (lambda (binding)
		 (member (car binding) slots))
	       '((sketch-title "Sketch")
		 (sketch-width 400)
		 (sketch-height 400)
		 (sketch-fullscreen nil)
		 (sketch-copy-pixels nil)
		 (sketch-y-axis :down)))))

(defun make-window-parameter-setf ()
  `(setf ,@(mapcan (lambda (binding)
		     `((,(car binding) instance) ,(car binding)))
		   (window-parameter-bindings nil))))

(defun make-custom-slots-setf (sketch bindings)
  `(setf ,@(mapcan (lambda (binding)
		     `((,(binding-accessor sketch binding) instance) ,(car binding)))
		   (custom-slots bindings))))

(defun make-reinitialize-setf ()
  `(setf ,@(mapcan (lambda (binding)
		     `((,(car binding) instance) (,(car binding) instance)))
		   (window-parameter-bindings nil))))

;;; DEFSKETCH macro

(defmacro defsketch (sketch-name bindings &body body)
  `(progn
     ,(make-channel-observers bindings)

     (defclass ,sketch-name (sketch)
       ,(sketch-bindings-to-slots `,sketch-name bindings))

     (defmethod prepare progn ((instance ,sketch-name) &rest initargs &key &allow-other-keys)
       (let* (,@(window-parameter-bindings bindings)
	      ,@(mapcar #'first-two
			(replace-channels-with-values bindings)))
	 ,(make-window-parameter-setf)
	 ,(make-custom-slots-setf sketch-name bindings)
	 (apply #'reinitialize-instance (list* instance initargs))
	 ,(make-reinitialize-setf)))

     (defmethod draw ((instance ,sketch-name) &key &allow-other-keys)
       (with-accessors ,(mapcar (lambda (x) (list (car x) (car x)))
				(window-parameter-bindings nil)) instance
	 (with-slots ,(mapcar #'car (custom-slots bindings)) instance
	   ,@body)))

     (defmethod initialize-instance :before ((instance ,sketch-name) &key &allow-other-keys)
       (initialize-sketch))))
