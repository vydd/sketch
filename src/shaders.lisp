;;;; shaders.lisp

(in-package #:sketch)

;;;  ____  _   _    _    ____  _____ ____  ____
;;; / ___|| | | |  / \  |  _ \| ____|  _ \/ ___|
;;; \___ \| |_| | / _ \ | | | |  _| | |_) \___ \
;;;  ___) |  _  |/ ___ \| |_| | |___|  _ < ___) |
;;; |____/|_| |_/_/   \_\____/|_____|_| \_\____/

(kit.gl.shader:defdict sketch-programs ()
  (kit.gl.shader:program :fill-shader (:view-m :model-m)
			 (:vertex-shader "
#version 330

uniform mat4 model_m;
uniform mat4 view_m;

layout (location = 0) in vec2 vertex;
layout (location = 1) in vec4 color;

smooth out vec4 f_color;

void main() {
    gl_Position = view_m * model_m * vec4(vertex, 0.0, 1.0);
    f_color = color;
}
")
			 (:fragment-shader "
#version 330

smooth in vec4 f_color;
out vec4 f_out;

void main() {
    f_out = f_color;
}
")))
