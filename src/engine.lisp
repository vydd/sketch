(in-package :sketch)

;;------------------------------------------------------------

(defvar *sketches* nil)

(defun ensure-sketch-is-initialized ()
  ;; {TODO} nasty ↓↓↓
  (when (cepl::uninitialized-p)
    (initialize-cepl)
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
  (ensure-sketch-is-initialized)
  (let ((ctx *cepl-context*)) ;; ← {TODO} make a new one
    (with-slots ((env %env) context viewport window width height y-axis) sketch
      (setf context ctx)
      (setf window (add-window ctx))
      (setf env (make-environment width height y-axis))
      (setf viewport (make-viewport (list width height)))
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
      (push sketch *sketches*)
      ;; Need to kick off thread to run #'main-loop
      )))

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
    (with-slots (cepl-window) window
      (make-surface-current context cepl-window))

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

;; hack, wont be needed when we we run these in threads
(defvar *running* nil)

(defun main-loop ()
  (loop :while *running* :do
     (loop :for sketch :in *sketches* :do
        (continuable (step-sketch sketch)))))
