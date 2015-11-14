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

(defstruct env
  ;; Drawing
  (pen nil)
  ;; Debugging
  (debug-key-pressed nil)
  (red-screen nil))

;;; Temporary, until done automatically by sdl2kit
(kit.sdl2:start)
(sdl2:in-main-thread ()
  (sdl2:gl-set-attr :context-major-version 4)
  (sdl2:gl-set-attr :context-minor-version 1)
  (sdl2:gl-set-attr :context-profile-mask 1))
;;;

(defparameter *env* (make-env))

(kit.gl.shader:defdict sketch-default-programs ()
  (kit.gl.shader:program :vertex-color (:view-m)
	   (:vertex-shader  "
#version 410

uniform mat4 view_m;

layout (location = 0) in vec2 vertex;
layout (location = 1) in vec4 color;

smooth out vec4 f_color;
smooth out vec2 pos;

void main() {
    gl_Position = view_m * vec4(vertex, 0.0, 1.0);
    pos = gl_Position.xy;
    f_color = color;
}
")
           (:fragment-shader "
#version 410

smooth in vec4 f_color;
smooth in vec2 pos;
out vec4 f_out;

void main() {
    f_out = f_color;
    f_out = vec4((pos.x+1)/2, pos.x, pos.x, 1.0);
}
")))

(defclass sketch (kit.sdl2:gl-window)
  (;; Timekeeping
   (start-time :initform (get-internal-real-time))
   (last-frame-time :initform (get-internal-real-time))
   (restart-sketch :initform t)
   ;; Window parameters
   (title :initform "Sketch")
   (framerate :initform :auto)
   (width :initform 200)
   (height :initform 200)
   (copy-pixels :initform nil)
   ;; GL
   (view-matrix :initform (kit.glm:ortho-matrix 0 200 200 0 -1 1))
   (programs :initform (kit.gl.shader:compile-shader-dictionary 'sketch-default-programs))))

(defun framelimit (window &optional (fps 60))
  "Limits the framerate by using sdl2:delay. Technically, it is not
the correct way to do things, but it will have to do for now."
  ;; Adapted from k-stz's code found in sdl2kit cube example. Used
  ;; with permission.
  (with-slots (last-frame-time) window
    (let ((elapsed-time (- (get-internal-real-time) last-frame-time))
	  (time-per-frame (/ internal-time-units-per-second fps)))
      (when (< elapsed-time time-per-frame)
	(sdl2:delay (floor (* 1000 (/ (- time-per-frame elapsed-time)
				      internal-time-units-per-second)))))
      (setf last-frame-time (get-internal-real-time)))))

(kit.gl.vao:defvao vertex-color-2d ()
  (:separate ()
	     (vertex :float 2)
	     (color :float 4)))

(defparameter *vao-verts*
  (make-array 6
   :element-type 'single-float
   :initial-contents #(100.0 50.0
                       50.0 150.0
                       150.0 150.0)))

;;; The colors:
(defparameter *vao-colors*
  (make-array 12
   :element-type 'single-float
   :initial-contents #(1.0 0.0 0.0 1.0
                       0.0 1.0 0.0 1.0
                       0.0 0.0 1.0 1.0)))

(defmethod kit.sdl2:render ((s sketch))
  (gl:clear-color 1.0 1.0 0.0 1.0)
  (gl:clear :color-buffer-bit)

  (let ((vao (make-instance 'kit.gl.vao:vao
			    :type 'vertex-color-2d
			    :primitive :triangles
			    :vertex-count (/ (length *vao-verts*) 2))))
    (kit.gl.vao:vao-buffer-vector vao 0 (* 4 (length *vao-verts*)) *vao-verts*)
    (kit.gl.vao:vao-buffer-vector vao 1 (* 4 (length *vao-colors*)) *vao-colors*)
    
    (with-slots (programs view-matrix) s
      (kit.gl.shader:use-program programs :vertex-color)
      (kit.gl.shader:uniform-matrix programs :view-m 4 (vector view-matrix))
      (kit.gl.vao:vao-draw vao)))
  #|
  (with-slots (width height framerate restart-sketch copy-pixels) s
    (cond (copy-pixels (gl:read-buffer :front)
		       (gl:draw-buffer :back)
		       (gl:copy-pixels 0 0 width height :color))
	  (t (gl:clear-color 0.0 1.0 0.0 1.0)
	     (gl:clear :color-buffer-bit)))    
    (when restart-sketch
      (handler-case
      	  (setup s)
      	(error ()
	  (progn
	    (gl:clear-color 1.0 1.0 0.0 1.0)
	    (gl:clear :color-buffer-bit))))
      (setf restart-sketch nil))
    (if (and (env-red-screen *env*)
	     (env-debug-key-pressed *env*))
	(progn
	  (setf (env-red-screen *env*) nil
		(env-debug-key-pressed *env*) nil)
	  (draw s))	
	(handler-case
	    (progn
	      (let ((red-screen (env-red-screen *env*)))
		(setf (env-red-screen *env*) nil
		      (env-debug-key-pressed *env*) nil)
		(draw s)
		(when red-screen
		  (setf restart-sketch t))))
	  (error ()
	    (progn
	      (gl:clear-color 1.0 0.0 0.0 1.0)
	      (gl:clear :color-buffer-bit)
	      (setf (env-red-screen *env*) t)))))    
    (when (not (equal framerate :auto))
      (framelimit s framerate)))
  |#


  )

(defmethod initialize-instance :after ((w sketch) &key &allow-other-keys)
  (with-slots (width height programs) w
    (setf (kit.sdl2:idle-render w) t)
    (sdl2:gl-set-swap-interval 1)
    (gl:viewport 0 0 width height)
    ;; ++ left out projection ++    
    (gl:enable :line-smooth)
    (gl:hint :line-smooth-hint :nicest)
    ;; ++ left out blending ++
    (gl:clear-color 0.0 0.0 0.0 1.0)
    (gl:clear :color-buffer-bit)
    (gl:clear :depth-buffer-bit)))

(defgeneric setup (sketch)
  (:documentation "Called before creating the sketch window.")
  (:method ((s sketch)) ()))

(defgeneric draw (sketch)
  (:documentation "Called repeatedly after creating the sketch window,
used for drawing.")
  (:method ((s sketch)) ()))

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
       
       ,(alexandria:when-let ((debug-scancode (getf window-options :debug nil)))
	  `(defmethod kit.sdl2:keyboard-event :before ((window ,sketch-name) s ts rp keysym)
	     (declare (ignore s ts rp))
	     (when (and (env-red-screen *env*)
			(sdl2:scancode= (sdl2:scancode-value keysym) ,debug-scancode))
	       (setf (env-debug-key-pressed *env*) t)))))))

(defmacro define-sketch-setup (sketch-name &body body)
  "Defines a sketch SETUP method. Body is wrapped with WITH-SLOTS for all slots defined. "
  `(defmethod setup ((window ,sketch-name))
     (with-slots ,(gethash sketch-name *sketch-slot-hash-table*) window
       ,@body)))



  #|

  Not sure what to do with these yet.

  (defmethod textinput-event :after ((window test-window) ts text)
  )

  (defmethod keyboard-event :after ((window test-window) state ts repeat-p keysym)
  )

  (defmethod mousewheel-event ((window simple-window) ts x y)
  )

  (defmethod textinput-event ((window simple-window) ts text)
  )

  (defmethod keyboard-event ((window simple-window) state ts repeat-p keysym)
  )

  (defmethod mousebutton-event ((window simple-window) state ts b x y)
  )

  (defmethod mousemotion-event ((window simple-window) ts mask x y xr yr)
  )

  |#
