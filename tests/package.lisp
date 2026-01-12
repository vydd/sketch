;;;; tests/package.lisp

(defpackage #:sketch-tests
  (:use #:cl #:fiveam)
  (:export #:run-tests))

(in-package #:sketch-tests)

(def-suite sketch-tests
  :description "All Sketch tests")

(defun run-tests ()
  (run! 'sketch-tests))
