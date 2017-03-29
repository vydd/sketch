;;;; geometry.lisp

(in-package #:sketch)

;;;   ____ _____ ___  __  __ _____ _____ ______   __
;;;  / ___| ____/ _ \|  \/  | ____|_   _|  _ \ \ / /
;;; | |  _|  _|| | | | |\/| |  _|   | | | |_) \ V /
;;; | |_| | |__| |_| | |  | | |___  | | |  _ < | |
;;;  \____|_____\___/|_|  |_|_____| |_| |_| \_\|_|

(defun edges (vertices &optional (closed t))
  (loop
     for i in (if closed
                  (append (last vertices) (butlast vertices))
                  (butlast vertices))
     for j in (if closed
                  vertices
                  (cdr vertices))
     collect (list i j)))

(defmacro with-lines (lines &body body)
  (flet ((i-to-s (i) (format nil "~a" i)))
    `(symbol-macrolet
         ,(loop
             for line in lines
             for i upfrom 0 by 2
             append
               (loop
                  for sym in '(x x y y)
                  for idx in '(1 2 1 2)
                  for line-accessor in '(caar caadr cadar cadadr)
                  collect
                    `(,(alexandria:symbolicate sym (i-to-s (+ i idx)))
                       (,line-accessor ,line))))
       ,@body)))

(defun translate-line (line d)
  (with-lines (line)
    (let* ((a (atan (- y2 y1) (- x2 x1)))
           (dx (* (sin a) d))
           (dy (* (cos a) d)))
      `((,(+ x1 dx) ,(- y1 dy)) (,(+ x2 dx) ,(- y2 dy))))))

(defun intersect-lines (line1 line2)
  ;; https://en.wikipedia.org/wiki/Line–line_intersection#Given_two_points_on_each_line
  ;; The algorithm is changed so that division by zero never happens.
  ;; The values that are returned for "intersection" may or may not make sense, but
  ;; having responsive but wrong sketch is much better than a red screen.
  (with-lines (line1 line2)
    (let* ((denominator (- (* (- x1 x2) (- y3 y4))
                           (* (- y1 y2) (- x3 x4))))
           (a (if (zerop denominator)
                  (/ (+ x2 x3) 2)
                  (/ (- (* (- (* x1 y2) (* y1 x2)) (- x3 x4))
                        (* (- (* x3 y4) (* y3 x4)) (- x1 x2)))
                     denominator)))
           (b (if (zerop denominator)
                  (/ (+ y2 y3) 2)
                  (/ (- (* (- (* x1 y2) (* y1 x2)) (- y3 y4))
                        (* (- (* x3 y4) (* y3 x4)) (- y1 y2)))
                     denominator))))
      (list a b))))

(defun grow-polygon (polygon d)
  (let ((polygon
         (mapcar (lambda (x) (apply #'intersect-lines x))
                 (edges (mapcar (lambda (x) (translate-line x (- d)))
                                (edges polygon))))))
    (append (cdr polygon) (list (car polygon)))))

(defun triangulate (polygon)
  (mapcar (lambda (point) (list (2d-geometry:x point) (2d-geometry:y point)))
          (apply #'append
                 (mapcar #'2d-geometry:point-list
                         (2d-geometry:decompose-complex-polygon-triangles
                          (apply #'2d-geometry:make-polygon-from-coords polygon))))))

(defun bounding-box (vertices)
  (let ((min-x) (min-y) (max-x) (max-y))
    (dolist (vertex vertices)
      (when (or (not min-x) (< (first vertex) min-x))
        (setf min-x (first vertex)))
      (when (or (not max-x) (> (first vertex) max-x))
        (setf max-x (first vertex)))
      (when (or (not min-y) (< (second vertex) min-y))
        (setf min-y (second vertex)))
      (when (or (not max-y) (> (second vertex) max-y))
        (setf max-y (second vertex))))
    (list (list min-x min-y) (list max-x max-y))))

(defun normalize-to-bounding-box (vertices)
  (let ((box (bounding-box vertices)))
    (with-lines (box)
      (mapcar (lambda (vertex)
                (list (normalize (first vertex) x1 x2)
                      (normalize (second vertex) y1 y2)))
              vertices))))
