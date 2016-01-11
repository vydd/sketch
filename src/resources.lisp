;;;; resources.lisp

(in-package #:sketch)

;;;  ____  _____ ____   ___  _   _ ____   ____ _____ ____
;;; |  _ \| ____/ ___| / _ \| | | |  _ \ / ___| ____/ ___|
;;; | |_) |  _| \___ \| | | | | | | |_) | |   |  _| \___ \
;;; |  _ <| |___ ___) | |_| | |_| |  _ <| |___| |___ ___) |
;;; |_| \_\_____|____/ \___/ \___/|_| \_\\____|_____|____/

(defclass resource () ())

(defclass image (resource)
  ((texture :initform nil :accessor image-texture :initarg :texture)
   (width :initform 0 :accessor image-width :initarg :width)
   (height :initform 0 :accessor image-height :initarg :height)))

(defun file-name-extension (name)
  (let ((pos (position #\. name :from-end t)))
    (when (numberp pos)
      (subseq name (1+ pos)))))

(defun load-resource (filename &key type force-reload)
  (symbol-macrolet ((resource (gethash key (env-resources *env*))))
    (let* ((key (alexandria:make-keyword (alexandria:symbolicate filename))))
      (when (or force-reload (not resource))
	(setf resource
	      (load-typed-resource
	       filename
	       (or type
		   (case (alexandria:make-keyword
			  (alexandria:symbolicate
			   (string-upcase (file-name-extension filename))))
		     ((:png :jpg :jpeg :tga :gif :bmp) :image)
		     ((:ttf :otf) :font))))))
      resource)))

(defgeneric load-typed-resource (filename type &key &allow-other-keys))

(defmethod load-typed-resource (filename type &key &allow-other-keys)
  (if (not type)
      (error (format nil "~a's type cannot be deduced." filename))
      (error (format nil "Unsupported resource type ~a" type))))

(defmethod load-typed-resource (filename (type (eql :image)) &key &allow-other-keys)
  (let ((texture (car (gl:gen-textures 1)))
        (surface (sdl2-image:load-image filename)))
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
