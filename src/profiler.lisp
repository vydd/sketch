;;;; profiler.lisp

(in-package #:sketch)

;;;  ____  ____   ___  _____ ___ _     _____ ____
;;; |  _ \|  _ \ / _ \|  ___|_ _| |   | ____|  _ \
;;; | |_) | |_) | | | | |_   | || |   |  _| | |_) |
;;; |  __/|  _ <| |_| |  _|  | || |___| |___|  _ <
;;; |_|   |_| \_\\___/|_|   |___|_____|_____|_| \_\

(defclass profiler ()
  ((frames-per-second :initform 0)
   (milliseconds-per-frame :initform 0)
   (triangles-per-second :initform 0)
   (total-frames :initform 0)
   (total-triangles :initform 0)))
