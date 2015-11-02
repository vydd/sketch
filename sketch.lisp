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

;;;  _____ _   ___     _____ ____   ___  _   _ __  __ _____ _   _ _____
;;; | ____| \ | \ \   / /_ _|  _ \ / _ \| \ | |  \/  | ____| \ | |_   _|
;;; |  _| |  \| |\ \ / / | || |_) | | | |  \| | |\/| |  _| |  \| | | |
;;; | |___| |\  | \ V /  | ||  _ <| |_| | |\  | |  | | |___| |\  | | |
;;; |_____|_| \_|  \_/  |___|_| \_\\___/|_| \_|_|  |_|_____|_| \_| |_|

(defstruct env
  ;; Timing
  (started nil)
  (ticks 0)
  (framerate 60)
  ;; Window
  (width 200)
  (height 200)
  (title "Sketch")
  ;; Rendering
  (smooth t)
  ;; Color
  (stroke nil)
  (fill nil))

(defparameter *env* (make-env))

(defun size (width height)
  (setf (env-width *env*) width
	(env-height *env*) height))

(defun title (str)
  (setf (env-title *env*) str))

(defun smooth ()
  (setf (env-smooth *env*) t))

(defun no-smooth ()
  (setf (env-smooth *env*) nil))

;;;  ____  _____ _____ _   _ ____
;;; / ___|| ____|_   _| | | |  _ \
;;; \___ \|  _|   | | | | | | |_) |
;;;  ___) | |___  | | | |_| |  __/
;;; |____/|_____| |_|  \___/|_|

(defun sketch-setup-opengl (win)
  (if (env-smooth *env*)
      (gl:enable :line-smooth)
      (gl:enable :polygon-smooth))
  (gl:viewport 0 0 (env-width *env*) (env-height *env*))
  (gl:matrix-mode :projection)
  (gl:ortho 0 (env-width *env*) (env-height *env*) 0 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (dotimes (i 2)
    (gl:clear-color 0.0 0.0 0.0 1.0)
    (gl:clear :color-buffer-bit)
    (gl:clear :depth-buffer-bit)
    (sdl2:gl-swap-window win)))

(defun nsketch-next-frame-p ()
  (let ((new-ticks (sdl2:get-ticks))
	(old-ticks (env-ticks *env*)))    
    (when (or (not (env-started *env*))
	      (< new-ticks old-ticks)
	      (>= new-ticks (+ old-ticks (/ 1000 (env-framerate *env*)))))
      (setf (env-started *env*) t)
      (setf (env-ticks *env*) new-ticks)
      t)))

(defun sketch (&key
		 ;; Window
		 (width 200)
		 (height 200)
		 (title "Sketch")
		 ;; Drawing
		 (setup (lambda () t))
		 (draw (lambda () t))
		 ;; Mouse
		 (mouse-clicked (lambda () t))
		 (mouse-dragged (lambda () t))
		 (mouse-moved (lambda () t))
		 (mouse-pressed (lambda () t))
		 (mouse-released (lambda () t))
		 (mouse-wheel (lambda () t))
		 ;; Keyboard
		 (key-pressed (lambda () t))
		 (key-released (lambda () t))
		 (key-typed (lambda () t))
		 ;; Timing
		 (framerate 60))

  (setf (env-title *env*) title)
  (setf (env-width *env*) width)
  (setf (env-height *env*) height)
  (setf (env-framerate *env*) framerate)
  
  (funcall setup)
  
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title (env-title *env*)
			   :w (env-width *env*)
			   :h (env-height *env*)
			   :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
	(sdl2:gl-make-current win gl-context)
	(sketch-setup-opengl win)
	
	(setf (env-ticks *env*) (sdl2:get-ticks))
	(setf (env-framerate *env*) framerate)
	(sdl2:with-event-loop (:method :poll)
	  (:idle ()
		 (when (nsketch-next-frame-p)
		   (gl:read-buffer :front)
		   (gl:draw-buffer :back)
		   (gl:copy-pixels 0 0 (env-width *env*) (env-height *env*) :color)
		   (funcall draw)
		   (gl:flush)
		   (sdl2:gl-swap-window win)))
	  (:quit () t))))))

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
  
(defun ngon (n a b c d &key (mode :corner) (pointy t))
  (let ((angle (if pointy -90 0)))
    (with-fill-and-stroke :triangle-fan
      (case mode
	(:corner       
	 (dotimes (phi n)
	   (gl:vertex (+ a (* c (cos (radians (+ angle (* (/ 360 n) phi))))))
		      (+ b (* d (sin (radians (+ angle (* (/ 360 n) phi)))))))))))))

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


(defparameter *width* 400)
(defparameter *height* 400)

(defparameter *sides* 1)
(defparameter *pen* (make-pen :fill (rgb 1.0 0.0 1.0)
			      :stroke (gray 1.0)))

;;;;;;;;;;;;;

(defun sketch-example-setup-1 ()
  (title "Sketch EXAMPLE 1")
  (size *width* *height*))

(defun sketch-example-draw-1 ()
  (setf *sides* (1+ (mod *sides* 20)))
  (background (gray 0.5))
  (with-pen *pen*
    (ngon *sides*
	  (/ *width* 2) (/ *height* 2)
	  (/ *width* 4) (/ *height* (/ *sides* 2)))))

(defun sketch-example-1 ()
  (sketch :setup #'sketch-example-setup
	  :draw #'sketch-example-draw
	  :framerate 20))

;;;;;;;;;;;;;

(defun sketch-example-setup-2 ()
  (title "Sketch EXAMPLE 2")
  (size *width* *height*))

(defun sketch-example-draw-2 ()
  (with-pen (make-pen :stroke (rgb (random 1.0) (random 1.0) (random 1.0))
		      :fill (rgb (random 1.0) (random 1.0) (random 1.0)))
    (case (random 3)
      (0 (ellipse (random *width*) (random *height*) (random 50) (random 50)))
      (1 (line (random *width*) (random *height*) (random *width*) (random *height*)))
      (2 (rect (random *width*) (random *height*) (random 50) (random 50) :mode :radius)))))

(defun sketch-example-2 ()
  (sketch :setup #'sketch-example-setup-2
	  :draw #'sketch-example-draw-2
	  :framerate 2))

;;;;;;;;;;;;;

(defun sketch-example-3 ()
  (let ((title "Sketch EXAMPLE 3 (Processing.org example / sinewave)")
	(width 640) (height 640)
	(pen (make-pen :fill (gray 1.0)))
	(xs 40)
	(steps 0))
    
    (sketch :title title :width width :height height :framerate 60
	    
	    :draw
	    (lambda ()
	      (incf steps)
	      (background (gray 0))
	      (with-pen pen
		(mapcar (lambda (x)
			  (ellipse (* x (/ width xs))
				   (+ (/ height 2)
				      (* (/ height 4)
					 (sin (* TWO_PI (/ (+ (/ steps 4) x) xs)))))
				   (/ width xs 3)
				   (/ width xs 3)))
			(alexandria:iota xs)))))))
