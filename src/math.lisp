;;;; math.lisp

(in-package #:sketch)

;;;  __  __    _  _____ _   _
;;; |  \/  |  / \|_   _| | | |
;;; | |\/| | / _ \ | | | |_| |
;;; | |  | |/ ___ \| | |  _  |
;;; |_|  |_/_/   \_\_| |_| |_|

;; Calculation

(defun clamp-1 (x)
  (alexandria:clamp x 0.0 1.0))

(defun normalize (x low high &key (clamp t) (out-low 0.0) (out-high 1.0))
  (let ((low (min low high))
        (high (max low high))
        (min-out-low (min out-low out-high))
        (min-out-high (max out-low out-high)))
    (let ((norm (+ out-low
                   (* (- out-high out-low)
                      (/ (- x low) (- high low))))))
      (if clamp (alexandria:clamp norm min-out-low min-out-high) norm))))

;; Trigonometry

(defconstant +pi+ PI)
(defconstant +two-pi+ (* PI 2))
(defconstant +tau+ +two-pi+)
(defconstant +half-pi+(/ PI 2))
(defconstant +quarter-pi+ (/ PI 4))
(defconstant +epsilon+ single-float-epsilon)
(defconstant +phi+ 1.61803398875)
(defconstant +golden-ratio+ +phi+)
(defconstant +e+ (exp 1))

(defun radians (deg)
  (* PI (/ deg 180)))

(defun degrees (rad)
  (* 180 (/ rad PI)))
