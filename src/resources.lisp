;;;; resources.lisp

(in-package #:sketch)

;;;  ____  _____ ____   ___  _   _ ____   ____ _____ ____
;;; |  _ \| ____/ ___| / _ \| | | |  _ \ / ___| ____/ ___|
;;; | |_) |  _| \___ \| | | | | | | |_) | |   |  _| \___ \
;;; |  _ <| |___ ___) | |_| | |_| |  _ <| |___| |___ ___) |
;;; |_| \_\_____|____/ \___/ \___/|_| \_\\____|_____|____/

;;; Classes

(defclass resource () ())

(defclass image (resource)
  ((texture :accessor image-texture :initarg :texture)
   (width :accessor image-width :initarg :width)
   (height :accessor image-height :initarg :height)))

(defclass font (resource)
  ((filename :accessor font-filename :initarg :filename)
   (pointer :accessor font-pointer :initarg :pointer)
   (size :accessor font-size :initarg :size)))

;;; Loading

(defun file-name-extension (name)
  ;; taken from dto's xelf code
  (let ((pos (position #\. name :from-end t)))
    (when (numberp pos)
      (subseq name (1+ pos)))))

(defun load-resource (filename &rest all-keys &key type force-reload &allow-other-keys)
  (symbol-macrolet ((resource (gethash key (env-resources *env*))))
    (let* ((key (alexandria:make-keyword
		 (alexandria:symbolicate filename (format nil "~a" all-keys)))))
      (when (or force-reload (not resource))
	(setf resource
	      (apply #'load-typed-resource
		     (list*  filename
			     (or type
				 (case (alexandria:make-keyword
					(alexandria:symbolicate
					 (string-upcase (file-name-extension filename))))
				   ((:png :jpg :jpeg :tga :gif :bmp) :image)
				   ((:ttf :otf) :font)))
			     all-keys))))
      resource)))

(defgeneric load-typed-resource (filename type &key &allow-other-keys))

(defmethod load-typed-resource (filename type &key &allow-other-keys)
  (if (not type)
      (error (format nil "~a's type cannot be deduced." filename))
      (error (format nil "Unsupported resource type ~a" type))))

(defun make-image-from-surface (surface)
  (let ((texture (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-image-2d :texture-2d 0 :rgba
		     (sdl2:surface-width surface)
		     (sdl2:surface-height surface)
		     0
		     :bgra
		     :unsigned-byte (sdl2:surface-pixels surface))
    (gl:bind-texture :texture-2d 0)
    (let ((image (make-instance 'image
				:width (sdl2:surface-width surface)
				:height (sdl2:surface-height surface)
				:texture texture)))
      (sdl2:free-surface surface)
      image)))

(defmethod load-typed-resource (filename (type (eql :image)) &key &allow-other-keys)
  (make-image-from-surface (sdl2-image:load-image filename)))

(defmethod load-typed-resource (filename (type (eql :font))
				&key (size 12) &allow-other-keys)
  (make-instance 'font
		 :filename filename
		 :pointer (sdl2-ttf:open-font filename size)
		 :size size))

;;; Sketch drawing functions

(defun image (image-resource x y &optional width height)
  (with-pen (make-pen :fill image-resource
		      :stroke (pen-stroke (env-pen *env*))
		      :weight (pen-weight (env-pen *env*)))
    (rect x
	  y
	  (or width (image-width image-resource))
	  (or height (image-height image-resource)))))

(defun text (text-string x y &optional width height)
  (let ((font (pen-font (env-pen *env*))))
    (when font
      (destructuring-bind (r g b a)
	  (typecase (pen-stroke (env-pen *env*))
	    (color (color-rgba-255 (pen-stroke (env-pen *env*))))
	    (image '(0 0 0 255)))
	(let* ((key (alexandria:make-keyword
		     (alexandria:symbolicate
		      text-string
		      (font-filename font)
		      (format nil "~a"
			      (list (font-size font) width height r g b a))))))
	  (symbol-macrolet ((resource (gethash key (env-resources *env*))))
	    (when t
	      (setf resource (make-image-from-surface
			      (sdl2-ttf:render-text-blended
			       (font-pointer font) text-string r g b a))))
	    (with-pen (make-pen :stroke nil)
	      (image resource x y width height))))))))
