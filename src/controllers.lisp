;;;; controllers.lisp

(in-package #:sketch)

;;;   ____ ___  _   _ _____ ____   ___  _     _     _____ ____  ____
;;;  / ___/ _ \| \ | |_   _|  _ \ / _ \| |   | |   | ____|  _ \/ ___|
;;; | |  | | | |  \| | | | | |_) | | | | |   | |   |  _| | |_) \___ \
;;; | |__| |_| | |\  | | | |  _ <| |_| | |___| |___| |___|  _ < ___) |
;;;  \____\___/|_| \_| |_| |_| \_\\___/|_____|_____|_____|_| \_\____/

;;; Mouse

(defparameter *buttons* '(:left nil :middle nil :right nil))

(defmethod on-click ((instance sketch) x y))
(defmethod on-middle-click ((instance sketch) x y))
(defmethod on-right-click ((instance sketch) x y))

(defmethod kit.sdl2:mousebutton-event ((instance sketch) state timestamp button x y)
  (let ((button (elt (list nil :left :middle :right) button))
	(method (elt (list nil #'on-click #'on-middle-click #'on-right-click) button)))
    (when (equal state :mousebuttondown)
      (setf (getf *buttons* button) t))
    (when (and (equal state :mousebuttonup) (getf *buttons* button))
      (setf (getf *buttons* button) nil)
      (funcall method instance x y))))

(defmethod kit.sdl2:mousemotion-event :after ((instance sketch)
                                              timestamp button-mask x y xrel yrel)
  (out :mouse (cons x y)
       :mouse-x x
       :mouse-y y
       :mouse-rel (cons xrel yrel)
       :mouse-xrel xrel
       :mouse-yrel yrel))

(defmethod kit.sdl2:mousewheel-event :after ((instance sketch)
                                             timestamp x y)
  (out :mouse-wheel (cons x y)
       :mouse-wheel-x x
       :mouse-wheel-y y))

(defmethod kit.sdl2:mousebutton-event :after ((instance sketch)
                                              state timestamp button x y)
  (with-slots (%env) instance
    (when (env-red-screen %env)
      (setf (env-debug-key-pressed %env) t))))

;;; Keyboard

(defmethod keyboard-event :after ((instance sketch)
                                  state timestamp repeatp keysym))
