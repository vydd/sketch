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

;;------------------------------------------------------------
;; Main

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
          (init))
      (error () (setf sdl2.kit::*started* nil)))))

(defun sdl2.kit::quit ()
  (sdl2:in-main-thread ()
    (setf sdl2.kit::*main-loop-quit* t))
  (quit))

;;------------------------------------------------------------
;; Window

(defmethod sdl2.kit::initialize-window progn
    ((window sdl2.kit::gl-window)
     &key (title "SDL2 Window") (x :centered) (y :centered) (w 800) (h 600)
       (shown t) resizable fullscreen flags &allow-other-keys)
  ;; {TODO} suppor these
  (declare (ignore x y))
  (format t "~%~%FLAGS: ~s~%~%" flags)
  (sdl2:in-main-thread ()
    ;; (case fullscreen
    ;;   ((nil))
    ;;   ((:windowed :desktop)
    ;;    (pushnew :fullscreen-desktop flags))
    ;;   (t (pushnew :fullscreen flags)))
    (with-slots (sdl2.kit::sdl-window sdl2.kit::gl-context) window
      (add-surface *cepl-context*
                   :title title
                   :width (truncate w)
                   :height (truncate h)
                   :fullscreen (not (null fullscreen))
                   :resizable resizable
                   :hidden (not shown)
                   :make-current t)
      (setf sdl2.kit::sdl-window
            (current-surface *cepl-context*))
      (setf sdl2.kit::gl-context
            (cepl.context::handle
             (slot-value *cepl-context*
                         'cepl.context::gl-context)))
      ;;
      ;; Setup GL defaults
      (setf (cull-face *cepl-context*) nil)
      (setf (depth-test-function *cepl-context*) nil)
      ;;
      (setf (gethash (sdl2:get-window-id sdl2.kit::sdl-window)
                     sdl2.kit::*all-windows*)
            window))))

(defmethod sdl2.kit::initialize-window progn
    ((window sdl2.kit::window) &key &allow-other-keys)
  ;; .silence.
  )

(defmethod sdl2.kit::render :before ((window sdl2.kit::gl-window))
  ;;(cls)
  (with-slots (sdl2.kit::sdl-window) window
    (make-surface-current *cepl-context* sdl2.kit::sdl-window)))

(defmethod sdl2.kit::render :after ((window sdl2.kit::gl-window))
  (when (autowrap:valid-p (sdl2.kit::sdl-window window))
    (gl:flush)
    ;;(print (get-internal-real-time))
    (swap)))

(defmethod sdl2.kit::window-event :before ((window sdl2.kit::gl-window) type ts d1 d2)
  (with-slots (sdl2.kit::sdl-window) window
    (make-surface-current *cepl-context* sdl2.kit::sdl-window)))
