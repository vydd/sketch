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
  (fill nil)
  (stroke nil))

;;; Temporary, until done automatically by sdl2kit
(kit.sdl2:start)
;;;

(defparameter *env* (make-env))

(defclass sketch (kit.sdl2:gl-window)
  (; Timekeeping
   (start-time :initform (get-internal-real-time))
   (last-frame-time :initform (get-internal-real-time))
   (restart-sketch :initform t)
   ; Window parameters
   (title :initform "Sketch")
   (framerate :initform 60)
   (width :initform 200)
   (height :initform 200)))

(defun framelimit (window &optional (fps 60))
  (with-slots (last-frame-time) window
    (let ((elapsed-time (- (get-internal-real-time) last-frame-time))
	  (time-per-frame (/ internal-time-units-per-second fps)))
      (when (< elapsed-time time-per-frame)
	(sdl2:delay (floor (* 1000 (/ (- time-per-frame elapsed-time)
				      internal-time-units-per-second)))))
      (setf last-frame-time (get-internal-real-time)))))

(defmethod kit.sdl2:render ((s sketch))
  (with-slots (width height framerate restart-sketch) s
    (gl:read-buffer :front)
    (gl:draw-buffer :back)
    (gl:copy-pixels 0 0 width height :color)
    (when restart-sketch
      (setup s)
      (setf restart-sketch nil))
    (draw s)
    (framelimit s framerate)))
;;
;; DEFSKETCH does this already. Will be added back when 
;; title/width/height/frame become reactive.
;;
;; (defun sketch-refresh-window (w)
;;   (let ((sdl-win (kit.sdl2:sdl-window w)))
;;     (with-slots (title width height framerate) w
;;       (sdl2:set-window-title sdl-win title)
;;       (sdl2:set-window-size sdl-win width height))))

(defmethod initialize-instance :after ((w sketch) &key &allow-other-keys)
  (setf (kit.sdl2:idle-render w) t)
  (sdl2:gl-set-swap-interval 1)
  (with-slots (width height) w
    (gl:viewport 0 0 width height)
    (gl:matrix-mode :projection)
    (gl:ortho 0 width height 0 -1 1)
    (gl:matrix-mode :modelview)
    (gl:load-identity))
  (gl:enable :line-smooth)
  (gl:hint :line-smooth-hint :nicest)
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit)
  (gl:clear :depth-buffer-bit))

(defgeneric setup (sketch)
  (:documentation "Called before creating the sketch window.")
  (:method ((s sketch)) ()))

(defgeneric draw (sketch)
  (:documentation "Called repeatedly after creating the sketch window,
used for drawing.")
  (:method ((s sketch)) ()))

(defmacro defsketch (name window-options slot-initform &body draw-body)
  `(alexandria:with-gensyms (s-title s-width s-height s-framerate)
     (progn
       (setf s-title (getf ',window-options :title "Sketch")
	     s-width (getf ',window-options :width 200)
	     s-height (getf ',window-options :height 200)
	     s-framerate (getf ',window-options :framerate 60))      
       (defclass ,name (sketch)
	 ,(append `((title :initform s-title)
		    (width :initform s-width)
		    (height :initform s-height)
		    (framerate :initform s-framerate)
		    (%%sketch-slots :initform (mapcar #'car ',slot-initform)))
		  (loop for arg in slot-initform
		     collecting (list (car arg) :initform (cadr arg)))))
       (defmethod draw ((window ,name))
	 (with-slots ,(append '(width height title framerate)
			      (mapcar #'car slot-initform)) window
	   ,@draw-body))
       (defmethod initialize-instance :after ((window ,name) &key &allow-other-keys)
		  (let ((sdl-win (kit.sdl2:sdl-window window)))
		    (sdl2:set-window-title sdl-win s-title)
		    (sdl2:set-window-size sdl-win s-width s-height))))))

(defmacro define-sketch-setup (name &body setup-body)
  `(defmethod setup ((window ,name))
     (with-slots ,(mapcar #'closer-mop:slot-definition-name
			  (closer-mop:compute-slots
			   (find-class name))) window
       ,@setup-body)))

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
