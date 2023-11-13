;;;; canvas.lisp

(in-package #:sketch)

;;;   ____    _    _   ___     ___    ____
;;;  / ___|  / \  | \ | \ \   / / \  / ___|
;;; | |     / _ \ |  \| |\ \ / / _ \ \___ \
;;; | |___ / ___ \| |\  | \ V / ___ \ ___) |
;;;  \____/_/   \_|_| \_|  \_/_/   \_|____/


(defclass canvas ()
  ((width :initarg :width :reader canvas-width)
   (height :initarg :height :reader canvas-height)
   (%image :initform nil :accessor %canvas-image)
   (%vector :initform nil :accessor %canvas-vector)
   (%locked :initform nil :accessor %canvas-locked)))

(defun make-canvas (width height)
  (let ((canvas (make-instance 'canvas :width width :height height)))
    (canvas-reset canvas)
    canvas))

(defmethod %canvas-vector-pointer ((canvas canvas))
  (static-vectors:static-vector-pointer (%canvas-vector canvas)))

(defmethod canvas-reset ((canvas canvas))
  (setf (%canvas-vector canvas)
        (static-vectors:make-static-vector (* (canvas-width canvas) (canvas-height canvas) 4) :initial-element 0)))

(defmethod canvas-paint ((canvas canvas) (color color) x y)
  (let ((ptr (%canvas-vector-pointer canvas))
        (pos (+ (* x 4) (* y 4 (canvas-width canvas))))
        (vec (color-bgra-255 color)))
    (dotimes (i 4)
      (setf (cffi:mem-aref ptr :uint8 (+ pos i)) (elt vec i)))))

(defmethod canvas-image ((canvas canvas)
			 &key (min-filter :linear)
                              (mag-filter :linear)
			 &allow-other-keys)
  (if (%canvas-locked canvas)
      (%canvas-image canvas)
      (make-image-from-surface
       (sdl2:create-rgb-surface-with-format-from
        (%canvas-vector-pointer canvas)
        (canvas-width canvas)
        (canvas-height canvas)
        32
        (* 4 (canvas-width canvas))
        :format sdl2:+pixelformat-argb8888+)
       :min-filter min-filter
       :mag-filter mag-filter)))

(defmethod canvas-lock ((canvas canvas)
			&key (min-filter :linear)
			     (mag-filter :linear)
			&allow-other-keys)
  (setf (%canvas-image canvas) (canvas-image canvas
					     :min-filter min-filter
					     :mag-filter mag-filter)
        (%canvas-locked canvas) t))

(defmethod canvas-unlock ((canvas canvas))
  (setf (%canvas-locked canvas) nil))

(defmethod draw ((canvas canvas)
                 &key (x 0) (y 0)
                   width height
                   (min-filter :linear)
                   (mag-filter :linear))
  "Draws a canvas with its top-left corner at co-ordinates X & Y. By default,
uses the width and height that the canvas was created with, but these can be
overwritten by parameters WIDTH and HEIGHT.

MIN-FILTER and MAG-FILTER are used to determine pixel colours when the
drawing area is smaller or larger, respectively, than the canvas. By default,
the :LINEAR function is used. :NEAREST is also a common option. Note that, if
CANVAS-LOCK is being used, then MIN-FILTER and MAG-FILTER should be passed
there instead.

See: https://registry.khronos.org/OpenGL-Refpages/gl4/html/glTexParameter.xhtml"
  (draw (canvas-image canvas :min-filter min-filter :mag-filter mag-filter)
        :x 0
        :y 0
        :width (or width (canvas-width canvas))
        :height (or height (canvas-height canvas))))
