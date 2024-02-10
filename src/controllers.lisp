;;;; controllers.lisp

(in-package #:sketch)

;;;   ____ ___  _   _ _____ ____   ___  _     _     _____ ____  ____
;;;  / ___/ _ \| \ | |_   _|  _ \ / _ \| |   | |   | ____|  _ \/ ___|
;;; | |  | | | |  \| | | | | |_) | | | | |   | |   |  _| | |_) \___ \
;;; | |__| |_| | |\  | | | |  _ <| |_| | |___| |___| |___|  _ < ___) |
;;;  \____\___/|_| \_| |_| |_| \_\\___/|_____|_____|_____|_| \_\____/

;;; Mouse

(defparameter *current-entity* nil)
(defparameter *current-sketch* nil)

(defun propagate-to-entity (sketch f x y &rest other-args)
  (loop
    for entity being the hash-key of (sketch-%entities sketch)
    for (im iw ih) being the hash-value of (sketch-%entities sketch)
    for (ix iy) = (transform-vertex (list x y) im)
    when (and (< 0 ix iw) (< 0 iy ih))
      ;; x & y arguments always come last.
      do (apply f entity (append other-args (list ix iy)))))

(defmacro def-xy-event-method (method-name args)
  "Defines a method for an event, as well as an :around method
that sets the sketch environment and propagates the event to entities.
x & y are assumed to come last in the argument list."
  `(progn
     (defmethod ,method-name (instance ,@args))
     (defmethod ,method-name :around ((*sketch* sketch) ,@args)
       (with-sketch (*sketch*)
         (let ((*draw-mode* nil))
           (propagate-to-entity *sketch* #',method-name x y
                                ,@(remove-if (lambda (arg) (member arg '(x y)))
                                             args))
           (call-next-method))))))

(def-xy-event-method on-click (x y))
(def-xy-event-method on-middle-click (x y))
(def-xy-event-method on-right-click (x y))
(def-xy-event-method on-mouse-button (button state x y))
(def-xy-event-method on-mouse-left (state x y))
(def-xy-event-method on-mouse-middle (state x y))
(def-xy-event-method on-mouse-right (state x y))
(def-xy-event-method on-mouse-left-up (x y))
(def-xy-event-method on-mouse-left-down (x y))
(def-xy-event-method on-mouse-middle-up (x y))
(def-xy-event-method on-mouse-middle-down (x y))
(def-xy-event-method on-mouse-right-up (x y))
(def-xy-event-method on-mouse-right-down (x y))
(def-xy-event-method on-hover (x y))
(defmethod on-enter (instance))
(defmethod on-leave (instance))

(defmethod on-hover :around ((entity entity) ix iy)
  (let ((*draw-mode* nil))
    (unless (eql *current-entity* entity)
      (on-leave *current-entity*)
      (setf *current-entity* entity)
      (on-enter entity))
    (call-next-method)))

(defmethod on-hover :around ((instance sketch) ix iy)
  (unless (eql *current-sketch* instance)
    (on-leave *current-sketch*)
    (setf *current-sketch* instance)
    (on-enter instance))
  (call-next-method))

(defmethod kit.sdl2:mousebutton-event ((instance sketch-window) state timestamp button x y)
  ;; For backward compatibility.
  (kit.sdl2:mousebutton-event (%sketch instance) state timestamp button x y)
  (on-mouse-button (%sketch instance)
                   (translate-sdl2-button button)
                   (translate-sdl2-button-state state)
                   x
                   y))

(defun translate-sdl2-button (button)
  (case button
    (1 :left)
    (2 :middle)
    (3 :right)
    (t button)))

(defun translate-sdl2-button-state (state)
  (case state
    (:mousebuttondown :down)
    (:mousebuttonup :up)
    (t state)))

(defmethod on-mouse-button :after ((instance sketch) button state x y)
  (case button
    (:left (on-mouse-left instance state x y))
    (:middle (on-mouse-middle instance state x y))
    (:right (on-mouse-right instance state x y))))

(defmacro def-on-mouse (button-name)
  (let ((method-name (alexandria:symbolicate 'on-mouse- button-name)))
    `(defmethod ,method-name :after ((instance sketch) state x y)
       (case state
         (:down (,(alexandria:symbolicate method-name '-down) instance x y))
         (:up (,(alexandria:symbolicate method-name '-up) instance x y))))))

(def-on-mouse left)
(def-on-mouse middle)
(def-on-mouse right)

(defmethod on-mouse-left-up :after ((instance sketch) x y)
  (on-click instance x y))
(defmethod on-mouse-middle-up :after ((instance sketch) x y)
  (on-middle-click instance x y))
(defmethod on-mouse-right-up :after ((instance sketch) x y)
  (on-right-click instance x y))

(defmethod kit.sdl2:mousemotion-event ((instance sketch-window) timestamp button-mask x y xrel yrel)
  ;; For backward compatibility.
  (kit.sdl2:mousemotion-event (%sketch instance) timestamp button-mask x y xrel yrel)
  (with-slots ((sketch %sketch)) instance
    (on-hover sketch x y)
    (unless
        (loop for entity being the hash-key of (sketch-%entities sketch)
              for (im iw ih) being the hash-value of (sketch-%entities sketch)
              for (ix iy) = (transform-vertex (list x y) im)
              when (and (< 0 ix iw) (< 0 iy ih))
                do (on-hover entity ix iy)
                   (return t))
      (when *current-entity*
        (on-leave *current-entity*)
        (setf *current-entity* nil)))))

(defmethod kit.sdl2:mousemotion-event :after ((instance sketch-window)
                                              timestamp button-mask x y xrel yrel)
  (out :mouse (cons x y)
       :mouse-x x
       :mouse-y y
       :mouse-rel (cons xrel yrel)
       :mouse-xrel xrel
       :mouse-yrel yrel))

(defmethod kit.sdl2:mousewheel-event :after ((instance sketch-window)
                                             timestamp x y)
  (out :mouse-wheel (cons x y)
       :mouse-wheel-x x
       :mouse-wheel-y y))

(defmethod kit.sdl2:mousebutton-event :after ((instance sketch-window)
                                              state timestamp button x y)
  (with-slots (%env) (%sketch instance)
    (when (env-red-screen %env)
      (when (eq state :mousebuttonup)
        (setf (env-debug-key-pressed %env) t)))))

;;; Keyboard

(defmethod on-text (instance text))
(defmethod on-key (instance key state))

(defmethod on-text :around ((*sketch* sketch) text)
  (with-sketch (*sketch*)
    (let ((*draw-mode* nil))
      (call-next-method))))

(defmethod on-key :around ((*sketch* sketch) key state)
  (with-sketch (*sketch*)
    (let ((*draw-mode* nil))
      (call-next-method))))

(defmethod kit.sdl2:textinput-event :after ((instance sketch-window) timestamp text)
  (on-text (%sketch instance) text))

(defmethod kit.sdl2:keyboard-event :after ((instance sketch-window) state timestamp repeat-p keysym)
  (when (not repeat-p)
    (on-key (%sketch instance)
            (without-sdl2-scancode-prefix keysym)
            (translate-sdl2-key-state state))))

(defun translate-sdl2-key-state (state)
  (case state
    (:keydown :down)
    (:keyup :up)
    (t state)))
