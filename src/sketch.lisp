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

;;; Sketch definition

(defclass sketch (kit.sdl2:gl-window)
  (;; Environment
   (env :initform (make-env))
   (restart-sketch :initform t)
   ;; Window parameters
   (title :initform "Sketch")
   (width :initform 400)
   (height :initform 400)
   (copy-pixels :initform nil)))

(defmethod initialize-instance :after ((sketch-window sketch)
				       &key &allow-other-keys)
  (initialize-environment sketch-window)
  (initialize-gl sketch-window))

(defgeneric setup (sketch)
  (:documentation "Called before creating the sketch window.")
  (:method ((sketch-window sketch)) ()))

(defgeneric draw (sketch)
  (:documentation "Called repeatedly after creating the sketch window,
used for drawing.")
  (:method ((sketch-window sketch)) ()))

(defgeneric handle-sketch-event (sketch event)
  (:documentation "Hooks into sketch to handle internal events, like
:FRAME-DRAW and :TRIANGLES-DRAW.")
  (:method ((sketch-window sketch) event) (declare (ignore event))))

;;; Rendering

(defmacro gl-catch (error-color &body body)
  `(handler-case
       (progn
	 ,@body)
     (error ()
       (progn
	 (background ,error-color)
	 (setf restart-sketch t
	       (env-red-screen *env*) t)))))

(defun draw-window (window)
  (start-draw)
  (draw window)
  (end-draw))

(defmethod kit.sdl2:render ((sketch-window sketch))
  (with-slots (env width height restart-sketch copy-pixels) sketch-window
    (with-environment env
      (with-pen *default-pen*
	(unless copy-pixels
	  (background (gray 0.4)))
	;; Restart sketch on setup and when recovering from an error.
	(when restart-sketch
	  (gl-catch (rgb 1 1 0)
	    (setup sketch-window))
	  (setf (slot-value sketch-window 'restart-sketch) nil))
	;; If we're in the debug mode, we exit from it immediately,
	;; so that the restarts are shown only once. Afterwards, we
	;; continue presenting the user with the red screen, waiting for
	;; the error to be fixed, or for the debug key to be pressed again.
	(if (debug-mode-p)
	    (progn
	      (exit-debug-mode)
	      (draw-window sketch-window))
	    (gl-catch (rgb 1 0 0)
	      (draw-window sketch-window))))))
  (handle-sketch-event sketch-window :frame-draw))

;;; Macros

(defparameter *sketch-slot-hash-table* (make-hash-table))

(defmacro defsketch (sketch-name window-options slot-bindings &body body)
  "Defines a class, inheriting from SKETCH:SKETCH. It is used for convenience
instead of defclass because it provides a compact syntax for declaring window
options, let-like init-form for providing slots and inline draw body. It also
takes care of communicating new title, sketch and height values to SDL backend.
Additionaly, defining a class using defsketch enables selected Sketch methods,
like DRAW and SETUP to automatically wrap their bodies inside WITH-SLOTS, using
all slot names."
  (let* ((sketch-title (getf window-options :title "Sketch"))
	 (sketch-width (getf window-options :width 400))
	 (sketch-height (getf window-options :height 400))
	 (sketch-copy-pixels (getf window-options :copy-pixels nil))
	 ;; We need to append SKETCH-TITLE, SKETCH-WIDTH, SKETCH-HEIGHT
	 ;; and SKETCH-COPY_PIXELS from WINDOW-OPTIONS to SLOT-BINDINGS.
	 ;; If SLOT-BINDINGS already contains any of these, we're going
	 ;; to replace them - declaring title, width, height or copy-pixels
	 ;; along with other slots is technically illegal in Sketch, but
	 ;; currently, we're just going to use the values provided inside
	 ;; WINDOW-OPTIONS, or fallback to defaults silently.
	 (slot-bindings
	  (append (remove-if
		   #'(lambda (x)
		       (member (car x)
			       '(title width height copy-pixels)))
		   slot-bindings)
		  `((title ,sketch-title)
		    (width ,sketch-width)
		    (height ,sketch-height)
		    (copy-pixels ,sketch-copy-pixels))))
	 (slots (mapcar #'car slot-bindings))
	 (initforms (mapcar #'(lambda (binding)
				`(,(car binding)
				   :initform ,(cadr binding)
				   :accessor ,(car binding)))
			    slot-bindings)))
    ;; We are going to need slot names available during macroexpansion, so that
    ;; our enhanced methods can know what slots should be provided to WITH-SLOTS.
    ;; This is accomplished by saving slot names provided via SLOT-BINDINGS and
    ;; WINDOW-OPTIONS into *SKETCH-SLOT-HASH-TABLE*.
    (setf (gethash sketch-name *sketch-slot-hash-table*) slots)

    ;; Sketch Initialization
    `(progn
       (defclass ,sketch-name (sketch)
	 ,initforms)

       (defmethod draw ((sketch-window ,sketch-name))
	 (with-slots ,(gethash sketch-name *sketch-slot-hash-table*) sketch-window
	   ,@body))

       (defmethod initialize-instance :after ((sketch-window ,sketch-name)
					      &key &allow-other-keys)
	 (let ((sdl-win (kit.sdl2:sdl-window sketch-window)))
	   (sdl2:set-window-title sdl-win ,sketch-title)
	   (sdl2:set-window-size sdl-win ,sketch-width ,sketch-height)))

       (defmethod initialize-instance :before ((sketch-window ,sketch-name)
					       &key &allow-other-keys)
	 ,(when sketch-copy-pixels
	   `(sdl2:gl-set-attr :doublebuffer 0)))

       ,(alexandria:when-let ((debug-scancode (getf window-options :debug nil)))
	   `(defmethod kit.sdl2:keyboard-event :before ((sketch-window ,sketch-name)
							state timestamp repeat-p keysym)
	     (declare (ignore state timestamp repeat-p))
	     (with-slots (env) sketch-window
	       (when (and (env-red-screen env)
			  (sdl2:scancode= (sdl2:scancode-value keysym) ,debug-scancode))
		 (setf (env-debug-key-pressed env) t))))))))

(defmacro define-sketch-setup (sketch-name &body body)
  "Defines a sketch SETUP method. Body is wrapped with WITH-SLOTS for all slots defined."
  `(defmethod setup ((sketch ,sketch-name))
     (with-slots ,(gethash sketch-name *sketch-slot-hash-table*) sketch
       ,@body)))
