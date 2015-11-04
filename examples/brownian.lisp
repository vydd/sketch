;;;; brownian.lisp

(in-package #:sketch-examples)

;;  ____  ____   _____        ___   _ ___    _    _   _
;; | __ )|  _ \ / _ \ \      / / \ | |_ _|  / \  | \ | |
;; |  _ \| |_) | | | \ \ /\ / /|  \| || |  / _ \ |  \| |
;; | |_) |  _ <| |_| |\ V  V / | |\  || | / ___ \| |\  |
;; |____/|_| \_\\___/  \_/\_/  |_| \_|___/_/   \_\_| \_|

(defsketch brownian-turtle
    (:title "Brownian turtle" :width 400 :height 400 :framerate 60)
    ((pos '(200 . 200)) (dir '(1 . 0))
     (pen (make-pen :stroke (gray 0)))
     (bg (gray 1))
     (len 6))
  (flet ((draw (paces)
	   (dotimes (i paces)
	     (let ((new-pos (cons (+ (car pos) (car dir))
				  (+ (cdr pos) (cdr dir)))))
	       (with-pen pen
		 (line (car pos) (cdr pos) (car new-pos) (cdr new-pos)))
	       (setf pos new-pos))))
	 (rotate (a)
	   (let ((a (+ a (degrees (atan (cdr dir) (car dir))))))
	     (setf dir (cons (cos (radians a))
			     (sin (radians a)))))))
    (rotate (* 45 (random 4)))
    (draw (+ (random len) len))))

(defmethod setup ((tt brownian-turtle))
  (with-slots (bg) tt
    (background bg)))
