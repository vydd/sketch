(in-package :sketch)

;;------------------------------------------------------------
;; Window

    ;; (case fullscreen
    ;;   ((nil))
    ;;   ((:windowed :desktop)
    ;;    (pushnew :fullscreen-desktop flags))
    ;;   (t (pushnew :fullscreen flags)))

;; (defmethod sdl2.kit::initialize-window progn
;;     ((window sdl2.kit::gl-window)
;;      &key (title "SDL2 Window") (x :centered) (y :centered) (w 800) (h 600)
;;        (shown t) resizable fullscreen flags &allow-other-keys)
;;   ;; {TODO} support these
;;   (declare (ignore x y))
;;   (format t "~%~%FLAGS: ~s~%~%" flags)
;;   (with-slots (sdl2.kit::sdl-window sdl2.kit::gl-context) window
;;     (add-surface *cepl-context*
;;                  :title title
;;                  :width (truncate w)
;;                  :height (truncate h)
;;                  :fullscreen (not (null fullscreen))
;;                  :resizable resizable
;;                  :hidden (not shown)
;;                  :make-current t)
;;     (setf sdl2.kit::sdl-window
;;           (current-surface *cepl-context*))
;;     (setf sdl2.kit::gl-context
;;           (cepl.context::handle
;;            (slot-value *cepl-context*
;;                        'cepl.context::gl-context)))
;;     ;;
;;     ;; Setup GL defaults
;;     (setf (cull-face *cepl-context*) nil)
;;     (setf (depth-test-function *cepl-context*) nil)
;;     ;;
;;     (setf (gethash (sdl2:get-window-id sdl2.kit::sdl-window)
;;                    sdl2.kit::*all-windows*)
;;           window)))

;;------------------------------------------------------------

(defvar *sketches* nil)


(defun ensure-sketch-is-initialized ()
  ;; {TODO} nasty ↓↓↓
  (unless (cepl::uninitialized-p)
    (initialize-cepl)
    (sdl2-ttf:init)))




(defvar *stepper*
  (temporal-functions:make-stepper (temporal-functions:seconds 1)))
(defvar *frames* 0)
(defvar *fps* 0)

(defun step-sketch (sketch)
  (incf *frames*)
  (when (funcall *stepper*)
    (setf *fps* *frames*)
    (setf *frames* 0))

  (with-slots (window) sketch
    ;; before any sdl2 window event
    ;;(make-surface-current *cepl-context* sdl2.kit::sdl-window)

    ;; render
    (with-slots (cepl-window) window
      (make-surface-current *cepl-context* cepl-window))

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
                  ;; continue presenting the user with the red screen, waiting for
                  ;; the error to be fixed, or for the debug key to be pressed again.
                  (if (debug-mode-p)
                      (progn
                        (exit-debug-mode)
                        (draw sketch))
                      (gl-catch (rgb 0.7 0 0) (draw sketch))))))))))

    (gl:flush) ;; {TODO} do we need this?
    (swap)))

(defun main-loop ()
  (loop :while *running* :do
     (loop :for sketch :in *sketches* :do
        (continuable (step-sketch sketch)))))
