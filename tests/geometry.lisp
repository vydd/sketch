;;;; tests/geometry.lisp

(in-package #:sketch-tests)

(def-suite geometry-tests
  :description "Tests for geometry utilities"
  :in sketch-tests)

(in-suite geometry-tests)

(defun approx= (a b &optional (tolerance 1e-6))
  (< (abs (- a b)) tolerance))

;;; edges tests

(test edges-closed-polygon
  (let ((edges (sketch::edges '((0 0) (1 0) (1 1) (0 1)))))
    (is (= 4 (length edges)))
    (is (equal '(((0 1) (0 0))
                 ((0 0) (1 0))
                 ((1 0) (1 1))
                 ((1 1) (0 1)))
               edges))))

(test edges-open-polyline
  (let ((edges (sketch::edges '((0 0) (1 0) (1 1)) nil)))
    (is (= 2 (length edges)))
    (is (equal '(((0 0) (1 0))
                 ((1 0) (1 1)))
               edges))))

;;; intersect-lines tests

(test intersect-lines-perpendicular
  (let* ((line1 '((0 0) (10 0)))   ; horizontal line y=0
         (line2 '((5 -5) (5 5)))   ; vertical line x=5
         (intersection (sketch::intersect-lines line1 line2)))
    (is (approx= 5 (first intersection)))
    (is (approx= 0 (second intersection)))))

(test intersect-lines-diagonal
  (let* ((line1 '((0 0) (10 10)))  ; y=x
         (line2 '((0 10) (10 0)))  ; y=-x+10
         (intersection (sketch::intersect-lines line1 line2)))
    (is (approx= 5 (first intersection)))
    (is (approx= 5 (second intersection)))))

(test intersect-lines-at-origin
  (let* ((line1 '((-5 0) (5 0)))   ; x-axis
         (line2 '((0 -5) (0 5)))   ; y-axis
         (intersection (sketch::intersect-lines line1 line2)))
    (is (approx= 0 (first intersection)))
    (is (approx= 0 (second intersection)))))

;;; bounding-box tests

(test bounding-box-square
  (let ((box (sketch::bounding-box '((0 0) (10 0) (10 10) (0 10)))))
    (is (equal '((0 0) (10 10)) box))))

(test bounding-box-triangle
  (let ((box (sketch::bounding-box '((5 0) (10 10) (0 10)))))
    (is (equal '((0 0) (10 10)) box))))

(test bounding-box-single-point
  (let ((box (sketch::bounding-box '((5 5)))))
    (is (equal '((5 5) (5 5)) box))))

(test bounding-box-negative-coords
  (let ((box (sketch::bounding-box '((-10 -10) (10 10) (0 0)))))
    (is (equal '((-10 -10) (10 10)) box))))

;;; normalize-to-bounding-box tests
;;; Note: depends on normalize from math.lisp

(test normalize-to-bounding-box-square
  (let ((normalized (sketch::normalize-to-bounding-box
                     '((0 0) (10 0) (10 10) (0 10)))))
    (is (= 4 (length normalized)))
    ;; First point should be (0,0) -> normalized to (0,0)
    (is (approx= 0 (first (first normalized))))
    (is (approx= 0 (second (first normalized))))
    ;; (10,10) should normalize to (1,1)
    (is (approx= 1 (first (third normalized))))
    (is (approx= 1 (second (third normalized))))))

;;; translate-line tests

(test translate-line-horizontal-up
  (let* ((line '((0 0) (10 0)))
         (translated (sketch::translate-line line 5)))
    ;; Translating a horizontal line by +5 should move it up (negative y in screen coords)
    (is (approx= 0 (caar translated)))
    (is (approx= -5 (cadar translated)))
    (is (approx= 10 (caadr translated)))
    (is (approx= -5 (cadadr translated)))))

(test translate-line-vertical-right
  (let* ((line '((0 0) (0 10)))
         (translated (sketch::translate-line line 5)))
    ;; Translating a vertical line by +5 should move it right
    (is (approx= 5 (caar translated)))
    (is (approx= 0 (cadar translated)))
    (is (approx= 5 (caadr translated)))
    (is (approx= 10 (cadadr translated)))))
