;;;; monkey-patching.lisp

(in-package #:sketch)

;;;
;;;
;;;
;;;
;;;
;;;
;;; Unlike SDL2Kit, CEPL doesnt come with an automatic approach to threading or
;;; context management. To get around this, we are moneky-patching sdl2kit to
;;; drive CEPL. Hopefully we can move to a better solution in future, but this
;;; works for now.

(defun sdl2.kit::init ()
  (unless sdl2::*wakeup-event*
    (setf sdl2::*wakeup-event* (sdl2::alloc 'sdl2-ffi:sdl-event)))
  (unless sdl2::*main-thread-channel*
    (sdl2::ensure-main-channel)

    ;; If we did not have a main-thread channel, make a default main
    ;; thread.
    #-(and ccl darwin)
    (setf sdl2::*the-main-thread*
          (bt:make-thread #'sdl2::sdl-main-thread :name "SDL2 Main Thread"))

    ;; On OSX, we need to run in the main thread; CCL allows us to
    ;; safely do this.  On other platforms (mainly GLX?), we just need
    ;; to run in a dedicated thread.
    #+(and ccl darwin)
    (let ((thread (find 0 (ccl:all-processes) :key #'ccl:process-serial-number)))
      (setf sdl2::*the-main-thread* thread)
      (ccl:process-interrupt thread #'sdl2::sdl-main-thread)))
  ;;
  (sdl2::in-main-thread (:no-event t)
    ;; HACK! glutInit on OSX uses some magic undocumented API to
    ;; correctly make the calling thread the primary thread. This
    ;; allows cl-sdl2 to actually work. Nothing else seemed to
    ;; work at all to be honest.
    #+(and ccl darwin)
    (cl-glut:init)
    (handler-case
      (unless (sdl2:was-init :everything)
        (cepl:init))
    (error () (setf sdl2.kit::*started* nil)))))

(defun sdl2.kit::quit ()
  (sdl2:in-main-thread ()
    (setf sdl2.kit::*main-loop-quit* t))
  (cepl:quit))
