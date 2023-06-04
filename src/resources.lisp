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

(defclass cropped-image (image)
  ((uv-rect :accessor cropped-image-uv-rect :initarg :uv-rect)))

(defun pixel-uv-rect (img x y w h)
  "Generate uv coordinates (0.0 to 1.0) for portion of IMG within
   the rect specified by X Y W H
   Image flipping can be done by using negative width and height values"
  (with-slots (width height) img
    (list (coerce-float (/ x width))
          (coerce-float (/ y height))
          (coerce-float (/ w width))
          (coerce-float (/ h height)))))

(defun cropped-image-from-image (image x y w h)
  (make-instance 'cropped-image
                 :texture (image-texture image)
                 :width w
                 :height h
                 :uv-rect (pixel-uv-rect image x y w h)))

(defclass typeface (resource)
  ((filename :accessor typeface-filename :initarg :filename)
   (pointer :accessor typeface-pointer :initarg :pointer)))

;;; Loading

(defun file-name-extension (name)
  ;; taken from dto's xelf code
  (let ((pos (position #\. name :from-end t)))
    (when (numberp pos)
      (subseq name (1+ pos)))))

(defun load-resource (filename &rest all-keys &key type force-reload-p &allow-other-keys)
  (let ((*env* (or *env* (make-env)))) ;; try faking env if we still don't have one
    (symbol-macrolet ((resource (gethash key (env-resources *env*))))
      (alexandria:remove-from-plistf all-keys :force-reload-p)
      (let* ((key (alexandria:make-keyword
                   (alexandria:symbolicate filename (format nil "~a" all-keys)))))
        (when force-reload-p
          (free-resource resource)
          (remhash key (env-resources *env*)))
        (when (not resource)
          (setf resource
                (apply #'load-typed-resource
                       (list*  filename
                               (or type
                                   (case (alexandria:make-keyword
                                          (alexandria:symbolicate
                                           (string-upcase (file-name-extension filename))))
                                     ((:png :jpg :jpeg :tga :gif :bmp) :image)
                                     ((:ttf :otf) :typeface)))
                               all-keys))))
        resource))))

(defgeneric load-typed-resource (filename type &key &allow-other-keys))

(defmethod load-typed-resource (filename type &key &allow-other-keys)
  (if (not type)
      (error (format nil "~a's type cannot be deduced." filename))
      (error (format nil "Unsupported resource type ~a" type))))

(defun make-image-from-surface (surface &key (free-surface t))
  (let ((texture (car (gl:gen-textures 1)))
        (rgba-surface (if (eq (sdl2:surface-format-format surface) sdl2:+pixelformat-rgba32+)
                          surface
                          (sdl2:convert-surface-format surface sdl2:+pixelformat-rgba32+))))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:pixel-store :unpack-row-length (/ (sdl2:surface-pitch rgba-surface) 4))
    (gl:tex-image-2d :texture-2d 0 :rgba
                     (sdl2:surface-width rgba-surface)
                     (sdl2:surface-height rgba-surface)
                     0
                     :rgba
                     :unsigned-byte (sdl2:surface-pixels rgba-surface))
    (gl:bind-texture :texture-2d 0)
    (let ((image (make-instance 'image
                                :width (sdl2:surface-width rgba-surface)
                                :height (sdl2:surface-height rgba-surface)
                                :texture texture)))
      (unless (eq rgba-surface surface) (sdl2:free-surface rgba-surface))
      (when free-surface (sdl2:free-surface surface))
      image)))

(defmethod load-typed-resource (filename (type (eql :image)) &key &allow-other-keys)
  (make-image-from-surface (sdl2-image:load-image filename)))

(defmethod load-typed-resource (filename (type (eql :typeface))
                                &key (size 18) &allow-other-keys)
  (make-instance 'typeface
                 :filename filename
                 :pointer (sdl2-ttf:open-font filename
                                              (coerce (truncate size)
                                                      '(signed-byte 32)))))

(defgeneric free-resource (resource))

(defmethod free-resource :around (resource)
  (when resource
    (call-next-method)))

(defmethod free-resource ((image image))
  (gl:delete-textures (list (image-texture image))))

(defmethod free-resource ((typeface typeface)))
