;;;; controllers.lisp

(in-package #:sketch)

;;;   ____ ___  _   _ _____ ____   ___  _     _     _____ ____  ____
;;;  / ___/ _ \| \ | |_   _|  _ \ / _ \| |   | |   | ____|  _ \/ ___|
;;; | |  | | | |  \| | | | | |_) | | | | |   | |   |  _| | |_) \___ \
;;; | |__| |_| | |\  | | | |  _ <| |_| | |___| |___| |___|  _ < ___) |
;;;  \____\___/|_| \_| |_| |_| \_\\___/|_____|_____|_____|_| \_\____/

;;; Mouse

(out :mouse '(0 . 0)
     :mouse-x 0
     :mouse-y 0
     :mouse-rel '(0 . 0)
     :mouse-xrel 0
     :mouse-yrel 0
     :mouse-button 0
     :mouse-wheel '(0 . 0)
     :mouse-wheel-x 0
     :mouse-wheel-y 0)

(defmethod kit.sdl2:mousemotion-event :after ((sketch-window sketch)
					      timestamp button-mask x y xrel yrel)
  (out :mouse (cons x y)
       :mouse-x x
       :mouse-y y
       :mouse-rel (cons xrel yrel)
       :mouse-xrel xrel
       :mouse-yrel yrel))

(defmethod kit.sdl2:mousewheel-event :after ((sketch-window sketch)
					     timestamp x y)
  (out :mouse-wheel (cons x y)
       :mouse-wheel-x x
       :mouse-wheel-y y))

(defmethod kit.sdl2:mousebutton-event :after ((sketch-window sketch)
					      state timestamp button x y)
  (out :mouse-button button))

;;; Keyboard

(defmethod keyboard-event :after ((sketch-window sketch)
				  state timestamp repeatp keysym)
  ()
  ;(out :pressed (sdl2: keysym))
  )

;;; Clock

  ;; Not sure what to do with these yet.

  ;; (defmethod textinput-event :after ((window test-window) ts text)
  ;; )

  ;; (defmethod keyboard-event :after ((window test-window) state ts repeat-p keysym)
  ;; )

  ;; (defmethod mousewheel-event ((window simple-window) ts x y)
  ;; )

  ;; (defmethod textinput-event ((window simple-window) ts text)
  ;; )

  ;; (defmethod keyboard-event ((window simple-window) state ts repeat-p keysym)
  ;; )

  ;; (defmethod mousebutton-event ((window simple-window) state ts b x y)
  ;; )

  ;; (defmethod mousemotion-event ((window simple-window) ts mask x y xr yr)
  ;; )
