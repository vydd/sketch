(in-package :sketch)

;;------------------------------------------------------------

(defvar *sketches* nil)

(defun ensure-sketch-is-initialized ()
  ;; {TODO} nasty ↓↓↓
  (when (cepl::uninitialized-p)
    (initialize-cepl))
  (when (= 0 (sdl2-ttf:was-init))
    (sdl2-ttf:init)))

;;------------------------------------------------------------
;; Window

;; (case fullscreen
;;   ((nil))
;;   ((:windowed :desktop)
;;    (pushnew :fullscreen-desktop flags))
;;   (t (pushnew :fullscreen flags)))

(defun add-window (context
                   &key (title "SDL2 Window")
                     (x :centered) (y :centered) (w 800) (h 600)
                     (shown t) resizable fullscreen flags &allow-other-keys)
  ;; {TODO} support these
  (declare (ignore x y))
  (format t "~%~%FLAGS: ~s~%~%" flags)
  (add-surface context
               :title title
               :width (truncate w)
               :height (truncate h)
               :fullscreen (not (null fullscreen))
               :resizable resizable
               :hidden (not shown)
               :make-current t)
  (first (surfaces context)))

;;------------------------------------------------------------

(defmethod initialize-instance :after ((sketch sketch)
                                       &rest initargs
                                       &key &allow-other-keys)
  ;;
  ;; Let's begin
  (ensure-sketch-is-initialized)
  ;;
  ;; Make some communication channels for our soon-to-be-created thread
  (let ((to-thread (make-instance 'chanl:unbounded-channel))
        (from-thread (make-instance 'chanl:unbounded-channel))
        (master-out-stream *standard-output*))
    ;;
    ;; make the thread which will be the main loop for the sketch
    (with-slots (thread) sketch
      (setf thread
            (bt:make-thread
             (lambda ()
               ;;
               ;; CEPL contexts are bound to 1 thread so make 1 for our
               ;; new thread
               (with-cepl-context
                   (ctx (cepl.context::make-context))
                 ;;
                 ;; Initialize all the internal data for our sketch
                 (with-slots ((env %env) context viewport window width height
                              y-axis to-thread-chanl from-thread-chanl)
                     sketch
                   (setf context ctx)
                   (setf window (add-window ctx))
                   (setf env (make-environment width height y-axis))
                   (setf viewport (make-viewport (list width height)))
                   (setf to-thread-chanl to-thread
                         from-thread-chanl from-thread)
                   ;;
                   (setf (clear-color ctx) (v! 0.0 1.0 0.0 1.0))
                   (setf (cull-face ctx) nil)
                   (setf (depth-test-function ctx) nil)
                   ;; {TODO} add these to CEPL
                   (gl:enable :line-smooth :polygon-smooth)
                   (gl:hint :line-smooth-hint :nicest)
                   (gl:hint :polygon-smooth-hint :nicest)
                   ;;
                   (apply #'prepare sketch initargs)
                   ;;
                   ;; Let the REPL thread know we are done with the setup
                   ;; it can now return the sketch object
                   (chanl:send from-thread :initialized)
                   ;;
                   ;; main loop
                   (let ((running t))
                     (loop :while running :do
                        (multiple-value-bind (msg from)
                            (chanl:recv from-thread :blockp nil)
                          (when from
                            (setf running
                                  (sketch-handle-thread-message sketch msg)))
                          (step-sketch sketch)))
                     (format master-out-stream
                             "## SKETCH LOOP FOR ~a HAS ENDED ##"
                             sketch))))))))
    (print (chanl:recv from-thread :blockp t))
    (push sketch *sketches*)
    sketch))

(defgeneric sketch-handle-thread-message (sketch msg))

(defmethod sketch-handle-thread-message ((sketch sketch) msg)
  ;; ignored messages
  (declare (ignore sketch msg)))

(defmethod update-instance-for-redefined-class :after
    ((instance sketch) added-slots discarded-slots property-list &rest initargs)
  (declare (ignore added-slots discarded-slots property-list))
  (apply #'prepare (list* instance initargs)))

;;------------------------------------------------------------

(defun step-sketch (sketch)
  (with-slots (context window) sketch
    ;; before any sdl2 window event
    ;;(make-surface-current context sdl2.kit::sdl-window)

    ;; render
    (make-surface-current context window)

    (with-slots (%env %restart width height copy-pixels viewport blending)
        sketch
      (with-blending blending
        (with-viewport viewport
          (with-environment %env
            (with-pen (make-default-pen)
              (with-font (make-default-font)
                (with-identity-matrix
                    (unless copy-pixels
                      (background (gray 0.4)))
                  ;; Restart sketch on setup and when recovering from an error.
                  (when %restart
                    (gl-catch (rgb 1 1 0.3) (setup sketch))
                    (setf (slot-value sketch '%restart) nil))
                  ;; If we're in the debug mode, we exit from it immediately,
                  ;; so that the restarts are shown only once. Afterwards, we
                  ;; continue presenting the user with the red screen, waiting
                  ;; for the error to be fixed, or for the debug key to be
                  ;; pressed again.
                  (if (debug-mode-p)
                      (progn
                        (exit-debug-mode)
                        (draw sketch))
                      (gl-catch (rgb 0.7 0 0) (draw sketch))))))))))

    (gl:flush) ;; {TODO} do we need this?
    (swap)))
