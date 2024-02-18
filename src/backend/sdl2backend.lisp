;;;; sdl2backend.lisp

(in-package #:sketch)

;;;  ____  ____  _     ____  ____    _    ____ _  _______ _   _ ____
;;; / ___||  _ \| |   |___ \| __ )  / \  / ___| |/ / ____| \ | |  _ \
;;; \___ \| | | | |     __) |  _ \ / _ \| |   | ' /|  _| |  \| | | | |
;;;  ___) | |_| | |___ / __/| |_) / ___ \ |___| . \| |___| |\  | |_| |
;;; |____/|____/|_____|_____|____/_/   \_\____|_|\_\_____|_| \_|____/

(let ((initialized nil))
  (defun initialize-backend ()
    (unless initialized
      (setf initialized t)
      (kit.sdl2:init)
      (sdl2-ttf:init)
      (sdl2:in-main-thread ()
        (sdl2:gl-set-attr :multisamplebuffers 1)
        (sdl2:gl-set-attr :multisamplesamples 4)

        (sdl2:gl-set-attr :context-major-version 3)
        (sdl2:gl-set-attr :context-minor-version 3)
        (sdl2:gl-set-attr :context-profile-mask 1)))))

;;; Backwards compatibility with starting the backend on sketch creation

(defmethod initialize-instance :around ((instance sketch) &key &allow-other-keys)
  (initialize-backend)
  (sdl2:in-main-thread ()
    (call-next-method))
  (kit.sdl2:start))
