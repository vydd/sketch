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
