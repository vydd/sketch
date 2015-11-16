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

(defclass sketch (kit.sdl2:gl-window)  
  (;; Environment
   (env :initform (make-env))
   (benchmarks :initform nil)
   ;; Timekeeping
   (start-time :initform (get-internal-real-time))
   (last-frame-time :initform (get-internal-real-time))
   (restart-sketch :initform t)
   ;; Window parameters
   (title :initform "Sketch")
   (framerate :initform :auto)
   (width :initform 200)
   (height :initform 200)
   (copy-pixels :initform nil)))

(defmethod initialize-instance :after ((w sketch) &key &allow-other-keys)
  (initialize-environment w)
  (initialize-gl w))

(defgeneric setup (sketch)
  (:documentation "Called before creating the sketch window.")
  (:method ((s sketch)) ()))

(defgeneric draw (sketch)
  (:documentation "Called repeatedly after creating the sketch window,
used for drawing.")
  (:method ((s sketch)) ()))

(defmacro gl-catch (error-color &body body)
  `(handler-case
       (progn
	 ,@body)
     (error ()
       (progn
	 (background ,error-color)
	 (setf restart-sketch t
	       (env-red-screen *env*) t)))))

(defun draw-all (s)
  (reset-buffers)
  (draw s)
  (draw-buffers))

(defmethod kit.sdl2:render ((s sketch))
  (with-slots (env width height framerate restart-sketch copy-pixels) s
    (with-environment env
      ;; On setup and when recovering from error, restart sketch.
      (when restart-sketch
	(gl-catch (rgb 1 1 0)
	  (setup s))
	(setf (slot-value s 'restart-sketch) nil))
      ;;
      (if (debug-mode-p)
	  (progn
	    (exit-debug-mode)
	    (draw-all s))
	  (gl-catch (rgb 1 0 0)
	    (draw-all s)))
      (when (not (equal framerate :auto))
	(framelimit s framerate)))))

;;; Macros

(defparameter *sketch-slot-hash-table* (make-hash-table))

(defmacro defsketch (sketch-name window-options slot-bindings &body body)
  "Defines a class, inheriting from SKETCH:SKETCH. It is used for convenience
instead of defclass because it provides a compact syntax for declaring window options,
let-like init-form for providing slots and inline draw body. It also takes care of
communicating new title, sketch and height values to SDL backend. Additionaly,
defining a class using defsketch enables selected Sketch methods, like DRAW and
SETUP to automatically wrap their bodies inside WITH-SLOTS, using all slot names."
  (let* ((sketch-title (getf window-options :title "Sketch"))
	 (sketch-width (getf window-options :width 200))
	 (sketch-height (getf window-options :height 200))
	 (sketch-framerate (getf window-options :framerate :auto))
	 (sketch-copy-pixels (getf window-options :copy-pixels nil))
	 ;; We need to append SKETCH-TITLE, SKETCH-WIDTH, SKETCH-HEIGHT
	 ;; and SKETCH-FRAMERATE from WINDOW-OPTIONS to SLOT-BINDINGS.
	 ;; If SLOT-BINDINGS already contains any of these, we're going
	 ;; to replace them - declaring title, width, height or framerate
	 ;; along with other slots is technically illegal in Sketch, but
	 ;; currently, we're just going to use the values provided inside
	 ;; WINDOW-OPTIONS, or fallback to defaults silently.
	 (slot-bindings
	  (append (remove-if
		   #'(lambda (x)
		       (member (car x)
			       '(title width height framerate copy-pixels)))
		   slot-bindings)
		  `((title ,sketch-title)
		    (width ,sketch-width)
		    (height ,sketch-height)
		    (framerate ,sketch-framerate)
		    (copy-pixels ,sketch-copy-pixels))))	 
	 (slots (mapcar #'car slot-bindings))	 
	 (initforms (mapcar #'(lambda (binding)
			       `(,(car binding) :initform ,(cadr binding)))
			    slot-bindings)))
    ;; We are going to need slot names available during macroexpansion, so that
    ;; our enhanced methods can know what slots should be provided to WITH-SLOTS.
    ;; This is accomplished by saving slot names provided via SLOT-BINDINGS and
    ;; WINDOW-OPTIONS into *SKETCH-SLOT-HASH-TABLE*.
    (setf (gethash sketch-name *sketch-slot-hash-table*) slots)
    
    `(progn
       (defclass ,sketch-name (sketch)
	 ,initforms)
       
       (defmethod draw ((window ,sketch-name))
	 (with-slots ,(gethash sketch-name *sketch-slot-hash-table*) window
	   ,@body))
       
       (defmethod initialize-instance :after ((window ,sketch-name) &key &allow-other-keys)
	 (let ((sdl-win (kit.sdl2:sdl-window window)))
	   (sdl2:set-window-title sdl-win ,sketch-title)
	   (sdl2:set-window-size sdl-win ,sketch-width ,sketch-height)))
       
       (defmethod initialize-instance :before ((w sketch) &key &allow-other-keys)
	 ,(when sketch-copy-pixels
	   `(sdl2:gl-set-attr :doublebuffer 0)))
       
       ,(alexandria:when-let ((debug-scancode (getf window-options :debug nil)))
	  `(defmethod kit.sdl2:keyboard-event :before ((window ,sketch-name) s ts rp keysym)
	     (declare (ignore s ts rp))
	     (with-slots (env) window
	       (when (and (env-red-screen env)
			  (sdl2:scancode= (sdl2:scancode-value keysym) ,debug-scancode))
		 (setf (env-debug-key-pressed env) t))))))))

(defmacro define-sketch-setup (sketch-name &body body)
  "Defines a sketch SETUP method. Body is wrapped with WITH-SLOTS for all slots defined. "
  `(defmethod setup ((window ,sketch-name))
     (with-slots ,(gethash sketch-name *sketch-slot-hash-table*) window
       ,@body)))
