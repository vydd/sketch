;;;; controllers.lisp

(in-package #:sketch)

;;;   ____ ___  _   _ _____ ____   ___  _     _     _____ ____  ____
;;;  / ___/ _ \| \ | |_   _|  _ \ / _ \| |   | |   | ____|  _ \/ ___|
;;; | |  | | | |  \| | | | | |_) | | | | |   | |   |  _| | |_) \___ \
;;; | |__| |_| | |\  | | | |  _ <| |_| | |___| |___| |___|  _ < ___) |
;;;  \____\___/|_| \_| |_| |_| \_\\___/|_____|_____|_____|_| \_\____/


;;; Mouse

(defparameter *buttons* (list :left nil :middle nil :right nil))
(defparameter *current-entity* nil)

(defmethod on-click (instance x y))
(defmethod on-middle-click (instance x y))
(defmethod on-right-click (instance x y))
(defmethod on-press (instance x y))
(defmethod on-middle-press (instance x y))
(defmethod on-right-press (instance x y))
(defmethod on-hover (instance x y))
(defmethod on-enter (instance))
(defmethod on-leave (instance))

(defun propagate-to-entity (sketch x y f)
  (loop
    for entity being the hash-key of (sketch-%entities sketch)
    for (im iw ih) being the hash-value of (sketch-%entities sketch)
    for (ix iy) = (transform-vertex (list x y) im)
    when (and (< 0 ix iw) (< 0 iy ih))
      do (funcall f entity ix iy)))

(defmacro def-xy-event-method (method-name)
  `(defmethod ,method-name :around ((*sketch* sketch) x y)
     (with-sketch (*sketch*)
       (let ((*draw-mode* nil))
         (propagate-to-entity *sketch* x y #',method-name)
         (call-next-method)))))

(def-xy-event-method on-click)
(def-xy-event-method on-middle-click)
(def-xy-event-method on-right-click)
(def-xy-event-method on-press)
(def-xy-event-method on-middle-press)
(def-xy-event-method on-right-press)
(def-xy-event-method on-hover)

(defmethod on-hover :around ((entity entity) ix iy)
  (let ((*draw-mode* nil))
    (unless (eql *current-entity* entity)
      (on-leave *current-entity*)
      (setf *current-entity* entity)
      (on-enter entity))
    (call-next-method)))

(defmethod kit.sdl2:mousebutton-event ((instance sketch-window) state timestamp button x y)
  ;; For backward compatibility.
  (kit.sdl2:mousebutton-event (sketch instance) state timestamp button x y)
  (with-slots (sketch) instance
    (let ((button (elt (list nil :left :middle :right) button))
          (click-method (elt (list nil #'on-press #'on-middle-press #'on-right-press) button))
          (release-method (elt (list nil #'on-click #'on-middle-click #'on-right-click) button)))
      (when (equal state :mousebuttondown)
        (setf (getf *buttons* button) t)
        (funcall click-method sketch x y))
      (when (and (equal state :mousebuttonup) (getf *buttons* button))
        (setf (getf *buttons* button) nil)
        (funcall release-method sketch x y)))))

(defmethod kit.sdl2:mousemotion-event ((instance sketch-window) timestamp button-mask x y xrel yrel)
  ;; For backward compatibility.
  (kit.sdl2:mousemotion-event (sketch instance) timestamp button-mask x y xrel yrel)
  (with-slots (sketch) instance
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
  (with-slots (%env) (sketch instance)
    (when (env-red-screen %env)
      (setf (env-debug-key-pressed %env) t))))

;;; Keyboard

(defconstant +scancode-prefix-length+ (length "scancode-"))

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
  (on-text (sketch instance) text))

(defmethod kit.sdl2:keyboard-event :after ((instance sketch-window) state timestamp repeat-p keysym)
  (when (not repeat-p)
    (on-key (sketch instance)
            ;; Removing the ugly "SCANCODE-" prefix from the keyword
            ;; symbol that denotes the button.
            (intern (subseq (symbol-name (sdl2:scancode keysym))
                            +scancode-prefix-length+)
                    (find-package "KEYWORD"))
            state)))

;;; Backward compatibility.
;; Previously, the main `sketch` class inherited from
;; `kit.sdl2:gl-window`, and input was handled by specialising on methods from
;; sdl2kit. So we need to forward sdl2kit input calls to the `sketch` class for
;; old sketches that rely on that approach.
(defmacro define-sdl2-forward (name (&rest args) &optional already-defined?)
  `(progn
     ;; An empty method so we don't get an error if we try to forward
     ;; when the user hasn't defined it.
     (defmethod ,name ((w sketch) ,@args))
     ,@(when (not already-defined?)
         `((defmethod ,name ((w sketch-window) ,@args)
             (,name (sketch w) ,@args)
             (call-next-method))))))
(define-sdl2-forward kit.sdl2:mousebutton-event (state timestamp button x y) t)
(define-sdl2-forward kit.sdl2:mousemotion-event (timestamp button-mask x y xrel yrel) t)
(define-sdl2-forward kit.sdl2:textinput-event (timestamp text))
(define-sdl2-forward kit.sdl2:keyboard-event (state timestamp repeatp keysym))
(define-sdl2-forward kit.sdl2:mousewheel-event (timestamp x y))
(define-sdl2-forward kit.sdl2:window-event (type timestamp data1 data2))
(define-sdl2-forward kit.sdl2:controller-added-event (c))
(define-sdl2-forward kit.sdl2:controller-removed-event (c))
(define-sdl2-forward kit.sdl2:controller-axis-motion-event (controller timestamp axis value))
(define-sdl2-forward kit.sdl2:controller-button-event (controller state timestamp button))
