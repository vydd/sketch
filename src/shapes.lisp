;;;; shapes.lisp

(in-package #:sketch)

;;;  ____  _   _    _    ____  _____ ____
;;; / ___|| | | |  / \  |  _ \| ____/ ___|
;;; \___ \| |_| | / _ \ | |_) |  _| \___ \
;;;  ___) |  _  |/ ___ \|  __/| |___ ___) |
;;; |____/|_| |_/_/   \_\_|   |_____|____/

(defun point (x y)
  (declare (type real x y))
  (let ((weight (or (pen-weight (env-pen *env*)) 1)))
    (with-pen (make-pen :fill (pen-stroke (env-pen *env*)))
      (circle x y (/ weight 2)))))

(defun make-line (x1 y1 x2 y2)
  (let* ((a (atan (- y2 y1) (- x2 x1)))
         (w (/ (or (pen-weight (env-pen *env*)) 1) 2))
         (dx (* (sin a) w))
         (dy (* (cos a) w)))
    (lambda ()
      (draw-shape
       :triangle-strip
       `((,(- x1 dx) ,(+ y1 dy))
         (,(- x2 dx) ,(+ y2 dy))
         (,(+ x1 dx) ,(- y1 dy))
         (,(+ x2 dx) ,(- y2 dy)))
       nil))))

(defun line (x1 y1 x2 y2)
  (declare (type real x1 y1 x2 y2))
  (with-pen (flip-pen (env-pen *env*))
    (funcall (make-line x1 y1 x2 y2))))

(defun translated-intersects (lines distance)
  (let ((lines (mapcar (lambda (x) (translate-line x distance)) lines)))
    (edges (append (list (caar lines))
                   (mapcar (lambda (x) (apply #'intersect-lines x))
                           (edges lines nil))
                   (cdar (last lines)))
           nil)))

(defun make-polyline (&rest coordinates)
  (multiple-value-bind (d+ d-)
      (div2-inexact (pen-weight (env-pen *env*)))
    (let* ((lines (edges (group coordinates) nil))
           (lefts (translated-intersects lines (+ d+)))
           (rights (translated-intersects lines (- d-))))
      (lambda ()
        (draw-shape
         :triangle-strip
         (mix-lists (apply #'append lefts)
                    (apply #'append rights))
         nil)))))

(defun polyline (&rest coordinates)
  (case (pen-weight (env-pen *env*))
    (nil nil)
    (1 (mapcar (lambda (x) (line (caar x) (cadar x) (caadr x) (cadadr x)))
               (edges (group coordinates) nil)))
    (t (with-pen (flip-pen (env-pen *env*))
         (funcall (apply #'make-polyline coordinates))))))

(defun make-rect (x y w h)
  (if (and (plusp w) (plusp h))
      (lambda ()
        (draw-shape
         :triangle-strip
         `((,x ,(+ y h)) (,x ,y) (,(+ x w) ,(+ y h)) (,(+ x w) ,y))
         `((,x ,y) (,x ,(+ y h)) (,(+ x w) ,(+ y h)) (,(+ x w) ,y))))
      (lambda ())))

(defun rect (x y w h)
  (declare (type real x y w h))
  (funcall (make-rect x y w h)))

(defun ngon-vertices (n cx cy rx ry &optional (angle 0))
  (let* ((angle (radians angle))
         (rx (if (zerop rx) +epsilon+ rx))
         (theta (/ +tau+ n))
         (tangential (tan theta))
         (radial (cos theta))
         (y-mul (/ ry rx)))
    (loop repeat n
          for x = (* (cos angle) rx) then (* radial (- x (* (- y) tangential)))
          and y = (* (sin angle) ry) then (* radial (- y (* x tangential)))
          collect `(,(+ x cx) ,(+ (* y-mul y) cy)))))

(defun make-ngon (n cx cy rx ry &optional (angle 0))
  (let ((vertices (ngon-vertices n cx cy rx ry angle)))
    (lambda ()
      (draw-shape :triangle-fan vertices vertices))))

(defun ngon (n cx cy rx ry &optional (angle 0))
  (declare (type fixnum n)
           (type real cx cy rx ry angle))
  (funcall (make-ngon n cx cy rx ry angle)))

(defun make-star (n cx cy ra rb &optional (angle 0))
  (let ((vertices (mix-lists (ngon-vertices n cx cy ra ra (+ 90 angle))
                             (ngon-vertices n cx cy rb rb (- (+ 90 angle) (/ 180 n))))))
    (lambda ()
      (draw-shape :triangle-fan
                  (list* (list cx cy)
                         (car (last vertices))
                         vertices)
                  vertices))))

(defun star (n cx cy ra rb &optional (angle 0))
  (declare (type fixnum n)
           (type real cx cy ra rb angle))
  (funcall (make-star n cx cy ra rb angle)))

(defun ellipse (cx cy rx ry)
  (declare (type real cx cy rx ry))
  (when (and (not (zerop rx)) (not (zerop ry)))
    (ngon (max 24 (truncate (* 5 (sqrt (/ (+ (abs rx) (abs ry)) 2)))))
          cx cy (abs rx) (abs ry))))

(defun circle (x y r)
  (declare (type real x y r))
  (when (not (zerop r))
    (ellipse x y (abs r) (abs r))))

(defclass polygon-tessellator (glu:tessellator)
  ((primitive :initform nil :accessor pt-primitive)
   (points :initform nil :accessor pt-points)
   (shapes :initform nil :accessor pt-shapes)))

(defmethod glu:begin-data-callback ((tess polygon-tessellator) primitive pdata)
  (declare (ignore pdata))
  (setf (pt-primitive tess) primitive
        (pt-points tess) nil))

(defmethod glu:vertex-data-callback ((tess polygon-tessellator) vdata pdata)
  (declare (ignore pdata))
  (push vdata (pt-points tess)))

(defmethod glu:end-data-callback ((tess polygon-tessellator) pdata)
  (declare (ignore pdata))
  (push (cons (pt-primitive tess) (nreverse (pt-points tess))) (pt-shapes tess))
  (setf (pt-primitive tess) nil
        (pt-points tess) nil))

(defmethod glu:combine-data-callback ((tess polygon-tessellator) coords-gl-array vdata-array weight-array pdata)
  (declare (ignore vdata-array weight-array pdata))
  (let ((coords-array (gl::gl-array-pointer coords-gl-array)))
    (list (cffi:mem-aref coords-array '%gl:double 0)
          (cffi:mem-aref coords-array '%gl:double 1))))

(defun draw-polygon (points)
  (let ((tobj (make-instance 'polygon-tessellator)))
    (glu:tess-property tobj :winding-rule :odd)
    (glu:with-tess-polygon (tobj)
      (glu:with-tess-contour tobj
        (loop for (x y) in points
              ;; Expects 3d coordinates.
              do (glu:tess-vertex tobj (list x y 0) (list x y)))))
    (glu:tess-delete tobj)
    ;; Callbacks (BEGIN-DATA, VERTEX-DATA, END-DATA) store series of
    ;; triangle primitives that should be used to draw the polygon.
    ;; They are represented by cons pairs (PRIMITIVE . POINTS). By the
    ;; end of tesselation they are stored in the SHAPES slot of the
    ;; tesselator object.
    ;;
    ;; FIXME: texture coordinates are being calculated based on the
    ;; bounding box. The bounding box might differ depending on how
    ;; the polygon is tessellated, which makes it not possible to use
    ;; textures as :FILL at the same time as drawing a POLYGON.
    (loop for (primitive . points) in (pt-shapes tobj)
          do (draw-shape primitive points nil))
    ;; Draws the contour of the polygon.
    (draw-shape nil nil points)))

(defun make-polygon (&rest coordinates)
  (lambda ()
    (draw-polygon (group coordinates))))

(defun polygon (&rest coordinates)
  (draw-polygon (group coordinates)))

(defun quadratic-bezier-point (v a b c)
  (let* ((d (lerp-lists v a b))
         (e (lerp-lists v b c)))
    (lerp-lists v d e)))

(defun cubic-bezier-point (v a b c d)
  (let* ((e (lerp-lists v a b))
         (f (lerp-lists v b c))
         (g (lerp-lists v c d)))
    (quadratic-bezier-point v e f g)))

(defun bezier (x1 y1 bx1 by1 bx2 by2 x2 y2)
  (declare (type real x1 y1 bx1 by1 bx2 by2 x2 y2))
  (let ((a (list x1 y1))
        (b (list bx1 by1))
        (c (list bx2 by2))
        (d (list x2 y2))
        (cs (max 2 (pen-curve-steps (env-pen *env*)))))
    (apply #'polyline
           (mapcan (lambda (v) (cubic-bezier-point v a b c d))
                   (alexandria:iota (1+ cs) :step (/ 1 cs))))))
