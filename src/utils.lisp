;;;; utils.lisp

(in-package #:sketch)

;;;  _   _ _____ ___ _     ____
;;; | | | |_   _|_ _| |   / ___|
;;; | | | | | |  | || |   \___ \
;;; | |_| | | |  | || |___ ___) |
;;;  \___/  |_| |___|_____|____/

(defun pad-list (list pad length)
  (if (>= (length list) length)
      list
      (append (make-list (- length (length list)) :initial-element pad)
	      list)))

(defun group-bits (x &key (bits 8))  
  (let ((bit-fill (1- (expt 2 bits))))
    (do* ((x x (ash x (- bits)))
	  (acc `(,(boole boole-and x bit-fill))
	       (cons (boole boole-and x bit-fill) acc)))
	 ((zerop x) (cdr acc)))))

(defun framelimit (window &optional (fps 60))
  "Limits the framerate by using sdl2:delay. Technically, it is not
the correct way to do things, but it will have to do for now."
  ;; Adapted from k-stz's code found in sdl2kit cube example. Used
  ;; with permission.
  (with-slots (last-frame-time) window
    (let ((elapsed-time (- (get-internal-real-time) last-frame-time))
	  (time-per-frame (/ internal-time-units-per-second fps)))
      (when (< elapsed-time time-per-frame)
	(sdl2:delay (floor (* 1000 (/ (- time-per-frame elapsed-time)
				      internal-time-units-per-second)))))
      (setf last-frame-time (get-internal-real-time)))))
