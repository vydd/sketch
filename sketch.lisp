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
  ((start-time :initform (get-internal-real-time))
   (one-frame-time :initform (get-internal-real-time))
   (frames :initform 0)
   (framerate :initform 60)
   (title :initform "Sketch")
   (width :initform 200)
   (height :initform 200)
   (stroke :initform nil)
   (fill :initform nil)))

(defun framelimit (window &optional (fps 60))
  (with-slots (one-frame-time) window
    (let ((elapsed-time (- (get-internal-real-time) one-frame-time))
          (time-per-frame (/ 1000.0 fps)))
      (when (< elapsed-time time-per-frame)
        (sdl2:delay (floor (- time-per-frame elapsed-time))))
      (setf one-frame-time (get-internal-real-time)))))

(defmethod kit.sdl2:render ((s sketch))  
  (gl:clear :color-buffer)
  (draw s)
  (framelimit s (slot-value s 'framerate)))

(defun sketch-refresh-window (sketch)
  (let ((sdl-win (kit.sdl2:sdl-window sketch)))
    (with-slots (title width height framerate) sketch
      (sdl2:set-window-title sdl-win title)
      (sdl2:set-window-size sdl-win width height))))

(defmethod initialize-instance :after ((w sketch) &key &allow-other-keys)
  (sketch-refresh-window w)
  (setf (kit.sdl2:idle-render w) t)
  (with-slots (width height) w
    (gl:viewport 0 0 width height)
    (gl:matrix-mode :projection)
    (gl:ortho 0 width height 0 -1 1)
    (gl:matrix-mode :modelview)
    (gl:load-identity))
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

;;;  _   _ _____ ___ _     ____
;;; | | | |_   _|_ _| |   / ___|
;;; | | | | | |  | || |   \___ \
;;; | |_| | | |  | || |___ ___) |
;;;  \___/  |_| |___|_____|____/

(defun pad-list (list pad length)
  (if (>= (length list) length)
      list
      (append (make-list (- length (length list)) :initial-element pad)
	      list)))

(defun group-bits (x &key (bits 8))  
  (let ((bit-fill (1- (expt 2 bits))))
    (do* ((x x (ash x (- bits)))
	  (acc `(,(boole boole-and x bit-fill))
	       (cons (boole boole-and x bit-fill) acc)))
	 ((zerop x) (cdr acc)))))

;;;  __  __    _  _____ _   _
;;; |  \/  |  / \|_   _| | | |
;;; | |\/| | / _ \ | | | |_| |
;;; | |  | |/ ___ \| | |  _  |
;;; |_|  |_/_/   \_\_| |_| |_|

;; Calculation

(defun clamp-1 (x)
  (alexandria:clamp x 0.0 1.0))

(defun normalize (x low high &key (clamp t) (out-low 0.0) (out-high 1.0))
  (let ((low (min low high))
	(high (max low high))
	(min-out-low (min out-low out-high))
	(min-out-high (max out-low out-high))
	)
    (let ((norm (+ out-low
		   (* (- out-high out-low)
		      (/ (- x low) (- high low))))))
      (if clamp (alexandria:clamp norm min-out-low min-out-high) norm))))

;; Trigonometry

(defconstant TWO_PI (* PI 2))
(defconstant TAU TWO_PI)
(defconstant HALF_PI (/ PI 2))
(defconstant QUARTER_PI (/ PI 4))

(defun radians (deg)
  (* PI (/ deg 180)))

(defun degrees (rad)
  (* 180 (/ rad PI)))

;;;   ____ ___  _     ___  ____
;;;  / ___/ _ \| |   / _ \|  _ \
;;; | |  | | | | |  | | | | |_) |
;;; | |__| |_| | |__| |_| |  _ <
;;;  \____\___/|_____\___/|_| \_\

;;; General

(defstruct color
  (red 0.0)
  (green 0.0)
  (blue 0.0)
  (hue 0.0)
  (saturation 0.0)
  (brightness 0.0)
  (alpha 1.0))

(defun rgb-to-hsb (r g b)
  (let* ((c-max (max r g b))
	 (c-min (min r g b))
	 (delta (- c-max c-min))
	 (hue (* 60 (cond ((= delta 0) 0)
			  ((= c-max r) (mod (/ (- g b) delta) 6))
			  ((= c-max g) (+ (/ (- b r) delta) 2))
			  ((= c-max b) (+ (/ (- r g) delta) 4)))))
	 (saturation (if (zerop c-max)
			 0
			 (/ delta c-max)))
	 (brightness c-max))
    (list (/ hue 360) saturation brightness)))

(defun hsb-to-rgb (h s b)
  (let* ((h (mod (* h 360) 360))
	 (c (* b s))
	 (x (* c (- 1 (abs (- (mod (/ h 60) 2) 1)))))
	 (m (- b c)))
    (mapcar (lambda (x) (+ m x))
	    (aref `#((,c ,x 0) (,x ,c 0) (0 ,c ,x)
		     (0 ,x ,c) (,x 0 ,c) (,c 0 ,x))
		  (floor (/ h 60))))))

(defun rgb (red green blue &optional (alpha 1.0))
  (destructuring-bind (red green blue alpha)
      (mapcar #'clamp-1 (list red green blue alpha))
    (let ((hsb (rgb-to-hsb red green blue)))
      (make-color :red red :green green :blue blue :alpha alpha
		  :hue (elt hsb 0) :saturation (elt hsb 1) :brightness (elt hsb 2)))))

(defun hsb (hue saturation brightness &optional (alpha 1.0))
  (destructuring-bind (hue saturation brightness alpha)
      (mapcar #'clamp-1 (list hue saturation brightness alpha))    
    (let ((rgb (hsb-to-rgb hue saturation brightness)))
      (make-color :hue hue :saturation saturation :brightness brightness :alpha alpha
		  :red (elt rgb 0) :green (elt rgb 1) :blue (elt rgb 2)))))

(defun gray (amount &optional (alpha 1.0))
  (rgb amount amount amount alpha))

(defun rgb-255 (r g b &optional (a 255))
  (rgb (/ r 255) (/ g 255) (/ b 255) (/ a 255)))

(defun hsb-360 (h s b &optional (a 255))
  (hsb (/ h 360) (/ s 100) (/ b 100) (/ a 255)))

(defun gray-255 (g &optional (a 255))
  (gray (/ g 255) (/ a 255)))

(defun hex-to-color (string)
  (destructuring-bind (r g b &optional (a 1.0))
      (let* ((bits (case (length string)
		     ((3 4) 4)
		     ((6 8) 8)
		     (t (error "~a is invalid hex color." string))))
	     (groups (group-bits (parse-integer string :radix 16 :junk-allowed t)
				 :bits bits)))
	(pad-list (mapcar (lambda (x) (/ x (if (= bits 4) 15 255))) groups)
		  0
		  (if (= 4 bits)
		      (length string)
		      (/ (length string) 2))))
    (rgb r g b a)))

(defun color-rgba (color)
  (list (color-red color)
	(color-green color)
	(color-blue color)
	(color-alpha color)))

(defun color-hsba (color)
  (list (color-hue color)
	(color-saturation color)
	(color-brightness color)
	(color-alpha color)))

(defun lerp-color (c1 c2 amount &key (mode :hsb))
  (let ((a (clamp-1 amount)))
    (flet ((norm (field)
	     (normalize a 0.0 1.0
			:out-low (slot-value c1 field) :out-high (slot-value c2 field))))
      (if (eq mode :hsb)
	  (apply #'hsb (mapcar #'norm '(hue saturation brightness alpha)))
	  (apply #'rgb (mapcar #'norm '(red green blue alpha)))))))

;;;  ____  _____ _   _
;;; |  _ \| ____| \ | |
;;; | |_) |  _| |  \| |
;;; |  __/| |___| |\  |
;;; |_|   |_____|_| \_|

(defstruct pen
  fill
  stroke)

(defmacro with-pen (pen &body body)
  (alexandria:once-only (pen)
    `(alexandria:with-gensyms (fill stroke)
       (progn
	 (setf fill (env-fill *env*)
	       stroke (env-stroke *env*)
	       (env-fill *env*) (pen-fill ,pen)
	       (env-stroke *env*) (pen-stroke ,pen))
	 ,@body
	 (setf (env-fill *env*) fill
	       (env-stroke *env*) stroke)))))

(defun fill-color (color)
  (setf (env-fill *env*) color))

(defun no-fill-color ()
  (setf (env-fill *env*) nil))

(defun stroke-color (color)
  (setf (env-stroke *env*) color))

(defun no-stroke-color ()
  (setf (env-stroke *env*) nil))

(defun background (color)
  (apply #'gl:clear-color (color-rgba color))
  (gl:clear :color-buffer-bit))

;;;  ____  _   _    _    ____  _____ ____
;;; / ___|| | | |  / \  |  _ \| ____/ ___|
;;; \___ \| |_| | / _ \ | |_) |  _| \___ \
;;;  ___) |  _  |/ ___ \|  __/| |___ ___) |
;;; |____/|_| |_/_/   \_\_|   |_____|____/

;;; 2D Primitives

(defmacro with-fill-and-stroke (primitive &body body)
  `(progn (when (env-fill *env*)
	    (apply #'gl:color (color-rgba (env-fill *env*)))
	    (gl:with-primitive ,primitive
	      ,@body))
	  (when (env-stroke *env*)
	    (apply #'gl:color (color-rgba (env-stroke *env*)))
	    (gl:with-primitive :line-loop
	      ,@body))))

(defun arc () t)

(defun ellipse (a b c d &key (mode :corner))
  (ngon 90 a b c d :mode mode))

(defun line (x1 y1 x2 y2)
  (when (env-stroke *env*)
    (apply #'gl:color (color-rgba (env-stroke *env*)))
    (gl:with-primitive :lines
      (gl:vertex x1 y1)
      (gl:vertex x2 y2))))

(defun point (x y &optional (z 0))
  (gl:with-primitive :points
    (gl:vertex x y z)))

(defun quad (x1 y1 x2 y2 x3 y3 x4 y4)
  (with-fill-and-stroke :quads
    (gl:vertex x1 y1)
    (gl:vertex x2 y2)
    (gl:vertex x3 y3)
    (gl:vertex x4 y4)))

(defun rect (a b c d &key (mode :corners))
  (case mode
    (:corners (quad a b c b c d a d))
    (:center (quad (- a (/ c 2)) (- b (/ d 2)) (+ a (/ c 2)) (- b (/ d 2))
		   (- a (/ c 2)) (+ b (/ d 2)) (+ a (/ c 2)) (+ b (/ d 2))))
    (:radius (quad (- a c) (- b d) (+ a c) (- b d)
		   (+ a c) (+ b d) (- a c) (+ b d)))))
  
(defun ngon (n a b c d &key (mode :corner) (angle 0))
  (with-fill-and-stroke :triangle-fan
    (case mode
      (:corner       
       (dotimes (phi n)
	 (gl:vertex (+ a (* c (cos (radians (+ angle (* (/ 360 n) phi))))))
		    (+ b (* d (sin (radians (+ angle (* (/ 360 n) phi))))))))))))

(defun triangle (x1 y1 x2 y2 x3 y3)
  (with-fill-and-stroke :triangles
    (gl:vertex x1 y1)
    (gl:vertex x2 y2)
    (gl:vertex x3 y3)))

;;;  _______  __    _    __  __ ____  _     _____ ____
;;; | ____\ \/ /   / \  |  \/  |  _ \| |   | ____/ ___|
;;; |  _|  \  /   / _ \ | |\/| | |_) | |   |  _| \___ \
;;; | |___ /  \  / ___ \| |  | |  __/| |___| |___ ___) |
;;; |_____/_/\_\/_/   \_\_|  |_|_|   |_____|_____|____/

(defclass sketch-example-1 (sketch)
  ((title :initform "Sketch example - 1")
   (width :initform 200)
   (height :initform 200)
   (framerate :initform 60)))

(defmethod draw ((s sketch-example-1))
  (background (gray 1.0))
  (with-pen (make-pen :fill (rgb 1.0 0.0 0.0) :stroke (rgb 0.0 0.0 1.0))
    (line 0 0 200 200)
    (ngon 6 100 100 50 50 :angle 0)))

;;;;;;

(defclass sketch-example-2 (sketch)
  ((title :initform "Sketch example - 2")
   (width :initform 400)
   (height :initform 400)
   (framerate :initform 60)
   (steps :initform 0)
   (xs :initform 40)
   (pen :initform (make-pen :fill (gray 1.0)))))

(defmethod draw ((s sketch-example-2))
  (with-slots (steps xs pen width height) s
    (incf steps)
    (background (gray 0.2))
    (with-pen pen
      (mapcar (lambda (x)
		(ellipse (* x (/ width xs))
			 (+ (/ height 2)
			    (* (/ height 4)
			       (sin (* TWO_PI (/ (+ (/ steps 4) x) xs)))))
			 (/ width xs 3)
			 (/ width xs 3)))
	      (alexandria:iota xs)))))

