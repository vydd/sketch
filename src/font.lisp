;;;; font.lisp

(in-package #:sketch)

;;;  _____ ___  _   _ _____
;;; |  ___/ _ \| \ | |_   _|
;;; | |_ | | | |  \| | | |
;;; |  _|| |_| | |\  | | |
;;; |_|   \___/|_| \_| |_|

(defclass font (resource)
  ((face :accessor font-face :initarg :face)
   (color :accessor font-color :initarg :color)
   (size :accessor font-size :initarg :size :initform 16)
   (line-height :accessor font-line-height :initarg :line-height :initform 1.41)
   (align :accessor font-align :initarg :align :initform :left)))

(defun make-font (&key face color size line-height align)
  (let* ((*env* (or *env* (make-env))))
    (make-instance 'font
                   :face (or face
                             (font-face (or (env-font *env*)
                                            (make-default-font))))
                   :color (or color +black+)
                   :size (or size 18)
		   :line-height (or line-height 1.41)
		   :align (or align :left))))

(defmacro with-font (font &body body)
  (alexandria:once-only (font)
    `(alexandria:with-gensyms (previous-font)
       (progn
         (setf previous-font (env-font *env*)
               (env-font *env*) ,font)
         ,@body
         (setf (env-font *env*) previous-font)))))

(defun set-font (font)
  (setf (env-font *env*) font))

(defun text-scale (resources spacing width height)
  (let ((rendered-width (apply #'max (mapcar #'image-width resources)))
	(rendered-height (+ (* (- (length resources) 1) spacing)
			    (apply #'+ (mapcar #'image-height resources)))))
    (cond ((and (not (numberp width)) (not (numberp height))) (list 1 1))
	  ((null width) (list 1 (/ height rendered-height)))
	  ((null height) (list (/ width rendered-width) 1))
	  ((eq :keep-ratio width) (list (/ height rendered-height) (/ height rendered-height)))
	  ((eq :keep-ratio height) (list (/ width rendered-width) (/ width rendered-width)))
	  (t (list (/ width rendered-width) (/ height rendered-height))))))

(defun text-align (align width)
  (cond ((eq align :right) (- width))
	((eq align :center) (- (round (/ width 2))))
	(t 0)))

(defun text (text-string x y &optional width height)
  (let* ((font (env-font *env*))
         (typeface (and font (load-resource (typeface-filename (font-face font))
                                            :size (font-size font)))))
    (when (and font (> (length text-string) 0))
      (with-pen (make-pen :stroke nil)
	(destructuring-bind (r g b a) (color-rgba-255 (font-color font))
	  (let* ((top 0)
		 (lines (split-sequence:split-sequence #\newline text-string))
		 (resources (mapcar (lambda (line)
				      (make-image-from-surface
				       (sdl2-ttf:render-utf8-blended
					(typeface-pointer typeface) line r g b a)))
				    lines))
		 (spacing (* (font-size font) (font-line-height font)))
		 (scale (text-scale resources spacing width height)))
	    (dolist (resource resources)
	      (image resource
		     (+ x (text-align (font-align font) (* (first scale) (image-width resource))))
		     (+ y top)
		     (* (first scale) (image-width resource))
		     (* (second scale) (image-height resource)))
	      (incf top (* (second scale) spacing))
	      (gl:delete-textures (list (image-texture resource))))))))))

(let ((font))
  (defun make-default-font ()
    (setf font (or font
                   (let ((filename (relative-path "res/sourcesans/SourceSansPro-Regular.otf")))
                     (make-font :face (make-instance 'typeface
                                                     :filename filename
                                                     :pointer (sdl2-ttf:open-font filename 18))
                                :color +black+
                                :size 18))))))

(let ((font))
  (defun make-error-font ()
    (setf font (or font
                   (let ((filename (relative-path "res/sourcesans/SourceSansPro-Regular.otf")))
                     (make-font :face (make-instance 'typeface
                                                     :filename filename
                                                     :pointer (sdl2-ttf:open-font filename 16))
                                :color +white+
                                :size 16))))))
