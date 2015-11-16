;;;; controller.lisp

(in-package #:sketch)

;;;   ____ ___  _   _ _____ ____   ___  _     _     _____ ____
;;;  / ___/ _ \| \ | |_   _|  _ \ / _ \| |   | |   | ____|  _ \
;;; | |  | | | |  \| | | | | |_) | | | | |   | |   |  _| | |_) |
;;; | |__| |_| | |\  | | | |  _ <| |_| | |___| |___| |___|  _ <
;;;  \____\___/|_| \_| |_| |_| \_\\___/|_____|_____|_____|_| \_\

;;; General

(defclass controller ()
  ((timestamp)))

(defgeneric subscribe-to-controller-event (()))

;;; Basic controllers

(defclass mouse (controller)
  (pointer-x pointer-y wheel-x wheel-y))


  #|

  Not sure what to do with these yet.

  (defmethod textinput-event :after ((window test-window) ts text)
  )

  (defmethod keyboard-event :after ((window test-window) state ts repeat-p keysym)
  )

  (defmethod mousewheel-event ((window simple-window) ts x y)
  )

  (defmethod textinput-event ((window simple-window) ts text)
  )

  (defmethod keyboard-event ((window simple-window) state ts repeat-p keysym)
  )

  (defmethod mousebutton-event ((window simple-window) state ts b x y)
  )

  (defmethod mousemotion-event ((window simple-window) ts mask x y xr yr)
  )

  |#
