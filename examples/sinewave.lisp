;;;; sinewave.lisp

(in-package #:sketch-examples)

;;;  ____ ___ _   _ _______        _____     _______
;;; / ___|_ _| \ | | ____\ \      / / \ \   / / ____|
;;; \___ \| ||  \| |  _|  \ \ /\ / / _ \ \ / /|  _|
;;;  ___) | || |\  | |___  \ V  V / ___ \ V / | |___
;;; |____/___|_| \_|_____|  \_/\_/_/   \_\_/  |_____|

(defsketch sinewave
    ((title "Sinewave") (width 400) (height 400)
     (steps 0) (xs (/ width 5)) (r 3))
  (incf steps)
  (background (rgb 0.2 0.2 0.2))
  (let ((w width) (h height))
    (flet ((sin-calc (x)
             (sin (* +tau+ (/ (+ (/ steps 4) x) xs)))))
      (dotimes (x xs)
        (with-pen (make-pen :fill (rgb (/ (1+ (sin-calc x)) 2)
                                       (/ (1+ (sin-calc (- x))) 2)
                                       0.2)
                            :stroke (gray 0.1))
          (ngon 6 (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (sin-calc x))) r r)
          (ngon 6 (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (sin-calc (- x)))) r r)
          (ngon 6 (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (- (sin-calc (- x))))) r r)
          (ngon 6 (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (- (sin-calc x)))) r r)
          (ngon 6 (* x (/ w xs)) (+ 20 (/ h 2) (* (/ h 4) (sin-calc x))) r r)
          (ngon 6 (* x (/ w xs)) (+ 20 (/ h 2) (* (/ h 4) (sin-calc (- x)))) r r)
          (ngon 6 (* x (/ w xs)) (+ 20 (/ h 2) (* (/ h 4) (- (sin-calc (- x))))) r r)
          (ngon 6 (* x (/ w xs)) (+ 20 (/ h 2) (* (/ h 4) (- (sin-calc x)))) r r)
          (ngon 6 (* x (/ w xs)) (+ 40 (/ h 2) (* (/ h 4) (sin-calc x))) r r)
          (ngon 6 (* x (/ w xs)) (+ 40 (/ h 2) (* (/ h 4) (sin-calc (- x)))) r r)
          (ngon 6 (* x (/ w xs)) (+ 40 (/ h 2) (* (/ h 4) (- (sin-calc (- x))))) r r)
          (ngon 6 (* x (/ w xs)) (+ 40 (/ h 2) (* (/ h 4) (- (sin-calc x)))) r r))))))


(let ((g496418 (find-class 'sinewave nil)))

  (unless g496418
    (defclass sinewave (sketch)
      ((steps :initarg :steps :accessor sinewave-steps)
       (xs :initarg :xs :accessor sinewave-xs)
       (r :initarg :r :accessor sinewave-r))))

  (defmethod sketch::prepare progn
    ((sketch::instance sinewave)
     &rest sketch::initargs
     &key &allow-other-keys)
    (declare (ignorable sketch::initargs))
    (let* (;; (title (slot-value sketch::instance 'title))
           ;; (width (slot-value sketch::instance 'width))
           ;; (height (slot-value sketch::instance 'height))
           (fullscreen (slot-value sketch::instance 'fullscreen))
           (copy-pixels (slot-value sketch::instance 'copy-pixels))
           (y-axis (slot-value sketch::instance 'y-axis))
           (title
            (if (getf sketch::initargs :title)
                (slot-value sketch::instance 'title)
                "sinewave"))
           (width
            (if (getf sketch::initargs :width)
                (slot-value sketch::instance 'width)
                400))
           (height
            (if (getf sketch::initargs :height)
                (slot-value sketch::instance 'height)
                400))
           (steps (or (getf sketch::initargs :steps) 0))
           (xs (or (getf sketch::initargs :xs) (/ width 5)))
           (r (or (getf sketch::initargs :r) 3)))
      (declare (ignorable title width height fullscreen copy-pixels y-axis))
      (setf (sketch-title sketch::instance) title
            (sketch-width sketch::instance) width
            (sketch-height sketch::instance) height
            (sketch-fullscreen sketch::instance) fullscreen
            (sketch-copy-pixels sketch::instance) copy-pixels
            (sketch-y-axis sketch::instance) y-axis)
      (setf (sinewave-steps sketch::instance) steps
            (sinewave-xs sketch::instance) xs
            (sinewave-r sketch::instance) r))
    (setf (sketch::env-y-axis-sgn (slot-value sketch::instance 'sketch::%env))
          (if (eq (slot-value sketch::instance 'y-axis) :down)
              1
              -1)))

  (when g496418
    (defclass sinewave (sketch)
      ((steps :initarg :steps :accessor sinewave-steps)
       (xs :initarg :xs :accessor sinewave-xs)
       (r :initarg :r :accessor sinewave-r))))

  (defmethod draw ((sketch::instance sinewave) &key &allow-other-keys)
    (with-accessors ((title sketch-title) (width sketch-width)
                     (height sketch-height) (fullscreen sketch-fullscreen)
                     (copy-pixels sketch-copy-pixels) (y-axis sketch-y-axis))
        sketch::instance
      (with-slots (title width height steps xs r)
          sketch::instance
        (incf steps)
        (background (rgb 0.2 0.2 0.2))
        (let ((w width) (h height))
          (flet ((sin-calc (x)
                   (sin (* +tau+ (/ (+ (/ steps 4) x) xs)))))
            ;;(break "sup2")
            (dotimes (x xs)
              (with-pen (make-pen :fill
                                  (rgb (/ (1+ (sin-calc x)) 2)
                                       (/ (1+ (sin-calc (- x))) 2) 0.2)
                                  :stroke (gray 0.1))
                (ngon 6 (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (sin-calc x))) r r)
                (ngon 6 (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (sin-calc (- x))))
                      r r)
                (ngon 6 (* x (/ w xs))
                      (+ (/ h 2) (* (/ h 4) (- (sin-calc (- x))))) r r)
                (ngon 6 (* x (/ w xs)) (+ (/ h 2) (* (/ h 4) (- (sin-calc x))))
                      r r)
                (ngon 6 (* x (/ w xs)) (+ 20 (/ h 2) (* (/ h 4) (sin-calc x)))
                      r r)
                (ngon 6 (* x (/ w xs))
                      (+ 20 (/ h 2) (* (/ h 4) (sin-calc (- x)))) r r)
                (ngon 6 (* x (/ w xs))
                      (+ 20 (/ h 2) (* (/ h 4) (- (sin-calc (- x))))) r r)
                (ngon 6 (* x (/ w xs))
                      (+ 20 (/ h 2) (* (/ h 4) (- (sin-calc x)))) r r)
                (ngon 6 (* x (/ w xs)) (+ 40 (/ h 2) (* (/ h 4) (sin-calc x)))
                      r r)
                (ngon 6 (* x (/ w xs))
                      (+ 40 (/ h 2) (* (/ h 4) (sin-calc (- x)))) r r)
                (ngon 6 (* x (/ w xs))
                      (+ 40 (/ h 2) (* (/ h 4) (- (sin-calc (- x))))) r r)
                (ngon 6 (* x (/ w xs))
                      (+ 40 (/ h 2) (* (/ h 4) (- (sin-calc x)))) r r))))))))
  (find-class 'sinewave))
