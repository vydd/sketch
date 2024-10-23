;;;; noise.lisp

(in-package #:sketch)

;;;  _   _  ____ _____  _____ ______ 
;;; | \ | |/ __ \_   _|/ ____|  ____|
;;; |  \| | |  | || | | (___ | |__   
;;; | . ` | |  | || |  \___ \|  __|  
;;; | |\  | |__| || |_ ____) | |____ 
;;; |_| \_|\____/_____|_____/|______|

(defun noise (&rest coordinates)
  (when *sketch*
    (if (null coordinates)
        0
        (let* ((noise-map (sketch-%noise-map *sketch*))
               (d (length coordinates))
               (state (gethash d noise-map)))
          (when (not state)
            (setf state (noisy:make-noise d
                                          :seed (sketch-%noise-seed *sketch*)
                                          :lod (sketch-%noise-lod *sketch*)
                                          :falloff (sketch-%noise-falloff *sketch*)))
            (setf (gethash d noise-map) state))
          (apply #'noisy:noise-gen state coordinates)))))

(defun noise-seed (seed)
  (when *sketch*
    (setf (sketch-%noise-seed *sketch*) seed)
    ;; So that the noise state for each number of dimensions is reinitialised
    ;; with the new seed.
    (reset-noise-map *sketch*)))

(defun noise-detail (&key lod falloff)
  (when *sketch*
    (when lod
      (setf (sketch-%noise-lod *sketch*) lod))
    (when falloff
      (setf (sketch-%noise-falloff *sketch*) falloff))
    (reset-noise-map *sketch*)))

(defun reset-noise-map (sketch)
  (setf (sketch-%noise-map sketch) (make-hash-table)))
