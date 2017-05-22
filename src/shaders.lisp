;;;; shaders.lisp

(in-package #:sketch)

;;;  ____  _   _    _    ____  _____ ____  ____
;;; / ___|| | | |  / \  |  _ \| ____|  _ \/ ___|
;;; \___ \| |_| | / _ \ | | | |  _| | |_) \___ \
;;;  ___) |  _  |/ ___ \| |_| | |___|  _ < ___) |
;;; |____/|_| |_/_/   \_\____/|_____|_| \_\____/

(defstruct-g sketch-vertex
  (pos :vec2 :accessor pos)
  (uv :vec2 :accessor uv)
  (color :uint8-vec4 :accessor color))

(defun-g fill-verts ((vert sketch-vertex)
                     &uniform (model-m :mat4) (view-m :mat4))
  (values (* view-m model-m (v! (pos vert) 0 1))
          (uv vert)
          (color vert)))

(defun-g fill-verts-frag ((uv :vec2) (color :vec4) &uniform (tex :sampler-2d))
  (* (texture tex uv) (/ color 255)))

(def-g-> fill-vertices-pline (:dynamic)
  (fill-verts sketch-vertex)
  (fill-verts-frag :vec2 :vec4))

(defun fill-primitive (sampler)
  (let* ((env *env*)
         (stream (env-vert-streamer env))
         (model-m (env-model-matrix env))
         (view-m (env-view-matrix env)))
    (map-g #'fill-vertices-pline stream
           :model-m model-m
           :view-m view-m
           :tex sampler)))
