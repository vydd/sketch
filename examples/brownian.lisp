;;;; brownian.lisp

(in-package #:sketch-examples)

;;  ____  ____   _____        ___   _ ___    _    _   _
;; | __ )|  _ \ / _ \ \      / / \ | |_ _|  / \  | \ | |
;; |  _ \| |_) | | | \ \ /\ / /|  \| || |  / _ \ |  \| |
;; | |_) |  _ <| |_| |\ V  V / | |\  || | / ___ \| |\  |
;; |____/|_| \_\\___/  \_/\_/  |_| \_|___/_/   \_\_| \_|

(defsketch brownian
    (:title "Brownian" :width 800 :height 600
	    :framerate :auto :copy-pixels t :debug :scancode-f1)
    ((pos '(400 . 300)) (dir '(1 . 0))
     (pen (make-pen :stroke (gray 0.5) :weight 1))
     (bg (gray 1))
     (len 3)
     (flower-pos nil)
     (flower-timer 30)
     (flower-color nil)
     (flower-size (+ 200 (random 200))))  
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
    (rotate (- (random 180) 90))
    (draw (+ (random len) len))
    (setf (car pos) (alexandria:clamp (car pos) -10 810)
	  (cdr pos) (alexandria:clamp (cdr pos) -10 610))
    (setf flower-timer (mod (+ flower-timer 1) flower-size))
    (when flower-pos
      (with-pen (make-pen :fill flower-color)
	(ellipse (car flower-pos) (cdr flower-pos) (/ flower-timer 40) (/ flower-timer 40))))
    (when (zerop flower-timer)
      (setf flower-pos pos
	    flower-color (random-color)
	    flower-size (+ 200 (random 200))))))

(define-sketch-setup brownian
  (background bg))
