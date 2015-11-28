;;;; drawing.lisp

(in-package #:sketch)

;;;  ____  ____      ___        _____ _   _  ____
;;; |  _ \|  _ \    / \ \      / /_ _| \ | |/ ___|
;;; | | | | |_) |  / _ \ \ /\ / / | ||  \| | |  _
;;; | |_| |  _ <  / ___ \ V  V /  | || |\  | |_| |
;;; |____/|_| \_\/_/   \_\_/\_/  |___|_| \_|\____|

(kit.gl.vao:defvao 2d-vertices ()
  (:separate ()
	     (vertex :float 2)))


(defun draw-sketch-primitive ()
  (kit.gl.vao:vao-bind vao)
  (gl:bind-buffer ))













(defun reset-buffers ()
  (dotimes (i (array-dimension *buffers* 0))
    (let ((buffer-type (aref *buffers* i 3)))
      (gl:bind-buffer buffer-type (1+ i))
      (%gl:buffer-data buffer-type
		       (* (aref *buffers* i 2) *vertex-count*)
		       (cffi:null-pointer)
		       :stream-draw)
      (setf (aref *buffers* i 0) (gl:map-buffer buffer-type :write-only)
	    (aref *buffers* i 1) 0))))

(defun draw-buffers ()
  (let ((vao (env-vao *env*)))      
    (kit.gl.vao:vao-bind vao)
    (%gl:draw-elements :triangles (aref *buffers* 2 1) :unsigned-int 0)
    (dotimes (i (array-dimension *buffers* 0))
      (gl:bind-buffer :array-buffer (- (array-dimension *buffers* 0) i))
      (gl:unmap-buffer :array-buffer)
      (setf (aref *buffers* i 0) nil))
    (gl:bind-buffer :array-buffer 0)))

(defmacro fill-buffer (idx f-type cl-type &rest vals)
  `(setf 
    ,@(loop
	 for i from 0 below (length vals)
	 for j in vals
	 append `((cffi:mem-aref
		   (aref *buffers* ,idx 0)
		   ,f-type
		   (+ (aref *buffers* ,idx 1) ,i))
		  (coerce ,j ,cl-type)))
    (aref *buffers* ,idx 1) (+ ,(length vals) (aref *buffers* ,idx 1))))

(defmacro push-vertices (&rest vals)
  (alexandria:with-gensyms (m00 m01 m03 m10 m11 m13)
    (macrolet ((mx (i j) ``(aref (env-model-matrix *env*) ,,(+ (* j 4) i))))
      `(let ((,m00 ,(mx 0 0)) (,m01 ,(mx 0 1)) (,m03 ,(mx 0 3))
	     (,m10 ,(mx 1 0)) (,m11 ,(mx 1 1)) (,m13 ,(mx 1 3)))
	 (fill-buffer 0 :float 'single-float
		      ,@(loop for (x y) in vals append
			     `((+ (* ,m00 ,x) (* ,m01 ,y) ,m03)
			       (+ (* ,m10 ,x) (* ,m11 ,y) ,m13))))))))

(defmacro push-colors (&rest vals)
  `(fill-buffer 1 :float 'single-float ,@vals))

(defmacro push-indices (&rest vals)
  (let ((delta (gensym)))
    `(let ((,delta (/ (aref *buffers* 0 1) 2)))
       (fill-buffer 2 :unsigned-int 'integer
		    ,@(loop for v in vals append
			   `((+ ,v ,delta)))))))

(defmacro push-color-struct (cs)
  `(push-colors
    (color-red ,cs) (color-green ,cs) (color-blue ,cs) (color-alpha ,cs)))

(defmacro push-fill (vertex-count)
  `(progn
     ,@(loop for i from 0 below vertex-count collect
	    `(push-color-struct (pen-fill (env-pen *env*))))))

(defmacro push-stroke (vertex-count)
  `(progn
     ,@(loop for i from 0 below vertex-count collect
	    `(push-color-struct (pen-stroke (env-pen *env*))))))

