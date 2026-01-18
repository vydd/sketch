;;;; tests/utils.lisp

(in-package #:sketch-tests)

(def-suite utils-tests
  :description "Tests for utility functions"
  :in sketch-tests)

(in-suite utils-tests)

;;; pad-list tests

(test pad-list-basic
  (is (equal '(0 0 1 2 3) (sketch::pad-list '(1 2 3) 0 5))))

(test pad-list-no-padding-needed
  (is (equal '(1 2 3) (sketch::pad-list '(1 2 3) 0 3))))

(test pad-list-list-longer-than-length
  (is (equal '(1 2 3 4 5) (sketch::pad-list '(1 2 3 4 5) 0 3))))

(test pad-list-custom-pad-element
  (is (equal '(x x 1 2) (sketch::pad-list '(1 2) 'x 4))))

;;; group tests

(test group-pairs
  (is (equal '((1 2) (3 4) (5 6)) (sketch::group '(1 2 3 4 5 6)))))

(test group-pairs-odd-length
  (is (equal '((1 2) (3 4)) (sketch::group '(1 2 3 4 5)))))

(test group-triples
  (is (equal '((1 2 3) (4 5 6)) (sketch::group '(1 2 3 4 5 6) 3))))

(test group-empty
  (is (equal nil (sketch::group nil))))

(test group-single-element
  (is (equal nil (sketch::group '(1)))))

;;; group-bits tests

(test group-bits-basic
  (is (equal '(1 0) (sketch::group-bits #x0100 8))))

(test group-bits-single-byte
  (is (equal '(255) (sketch::group-bits #xff 8))))

(test group-bits-multi-byte
  (is (equal '(1 2 3) (sketch::group-bits #x010203 8))))

(test group-bits-4-bit
  (is (equal '(1 2 3 4) (sketch::group-bits #x1234 4))))

(test group-bits-zero
  (is (equal nil (sketch::group-bits 0 8))))

;;; order-list tests

(test order-list-basic
  (is (equal '(c a b) (sketch::order-list '(2 0 1) '(a b c)))))

(test order-list-repeat
  (is (equal '(a a a) (sketch::order-list '(0 0 0) '(a b c)))))

;;; mix-lists tests

(test mix-lists-basic
  (is (equal '(1 a 2 b 3 c) (sketch::mix-lists '(1 2 3) '(a b c)))))

(test mix-lists-empty
  (is (equal nil (sketch::mix-lists nil nil))))

;;; div2-inexact tests

(test div2-inexact-even
  (multiple-value-bind (low high) (sketch::div2-inexact 10)
    (is (= 5 low))
    (is (= 5 high))))

(test div2-inexact-odd
  (multiple-value-bind (low high) (sketch::div2-inexact 11)
    (is (= 5 low))
    (is (= 6 high))))

;;; lerp-lists tests

(test lerp-lists-basic
  (let ((result (sketch::lerp-lists 0.5 '(0.0 0.0) '(10.0 20.0))))
    (is (= 5.0 (first result)))
    (is (= 10.0 (second result)))))

(test lerp-lists-at-zero
  (let ((result (sketch::lerp-lists 0.0 '(0.0 0.0) '(10.0 20.0))))
    (is (= 0.0 (first result)))
    (is (= 0.0 (second result)))))

(test lerp-lists-at-one
  (let ((result (sketch::lerp-lists 1.0 '(0.0 0.0) '(10.0 20.0))))
    (is (= 10.0 (first result)))
    (is (= 20.0 (second result)))))

;;; abs-or-rel tests

(test abs-or-rel-fraction
  (is (= 50 (sketch::abs-or-rel 0.5 100))))

(test abs-or-rel-absolute
  (is (= 50 (sketch::abs-or-rel 50 100))))

(test abs-or-rel-zero-or-negative
  (is (= 100 (sketch::abs-or-rel 0 100)))
  (is (= 100 (sketch::abs-or-rel -1 100))))

;;; flatten tests

(test flatten-basic
  (is (equal '(1 2 3 4) (sketch::flatten '((1 2) (3 4))))))

(test flatten-nested
  (is (equal '(1 2 3 4 5) (sketch::flatten '((1 (2 3)) (4 5))))))

(test flatten-empty
  (is (equal nil (sketch::flatten nil))))

;;; coerce-float tests

(test coerce-float-from-integer
  (is (typep (sketch::coerce-float 1) 'single-float))
  (is (= 1.0 (sketch::coerce-float 1))))

(test coerce-float-from-double
  (is (typep (sketch::coerce-float 1.0d0) 'single-float)))
