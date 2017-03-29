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
   (size :accessor font-size :initarg :size)))

(defun make-font (&key face color size)
  (let* ((*env* (or *env* (make-env))))
    (make-instance 'font
                   :face (or face
                             (font-face (or (env-font *env*)
                                            (make-default-font))))
                   :color (or color +black+)
                   :size (or size 18))))

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

(defun text (text-string x y &optional width height)
  (let* ((font (env-font *env*))
         (typeface (and font (load-resource (typeface-filename (font-face font))
                                            :size (font-size font)))))
    (when font
      (destructuring-bind (r g b a) (color-rgba-255 (font-color font))
        (let ((resource (make-image-from-surface
                         (sdl2-ttf:render-utf8-blended
                          (typeface-pointer typeface) text-string r g b a))))
          (with-pen (make-pen :stroke nil)
            (image resource x y width height)
            (gl:delete-textures (list (image-texture resource)))))))))

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
