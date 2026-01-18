# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Sketch is a Common Lisp framework for creative coding and visual design, inspired by Processing. It provides an immediate-mode graphics API built on SDL2 and OpenGL 3.3+.

## Build and Run Commands

```lisp
;; Load the library (from REPL with Quicklisp)
(ql:quickload :sketch)

;; Load and run examples
(ql:quickload :sketch-examples)
(make-instance 'sketch-examples:hello-world)

;; Run tests
(ql:quickload :sketch-tests)
(sketch-tests:run-tests)
```

## Architecture

### Core Abstractions

- **`defsketch` macro** (`src/sketch.lisp`): Main entry point. Creates a class that inherits from `sketch` with a drawing body that runs at ~60fps. The body is Lisp code with direct calls to drawing functions.

- **Environment** (`src/environment.lisp`): The `env` struct holds all rendering state (pen, font, matrix stack, textures). Accessed via `*env*` special variable during drawing.

- **Pen** (`src/pen.lisp`): Drawing attributes (stroke color, fill color, weight, curve-steps, winding-rule). Use `make-pen` and `with-pen` macro.

- **Transforms** (`src/transforms.lisp`): Matrix-based 2D transforms (`translate`, `rotate`, `scale`) with stack operations (`push-matrix`, `pop-matrix`). Scoped versions use `with-translate`, `with-rotate`, `with-scale`.

### Module Organization

| Module | Purpose |
|--------|---------|
| `sketch.lisp` | Sketch class, `defsketch` macro, window lifecycle |
| `shapes.lisp` | Primitive shapes: `rect`, `circle`, `ellipse`, `line`, `polygon`, `bezier`, `ngon` |
| `color.lisp` | Color models (RGB/HSB), color constants (`+red+`, `+blue+`, etc.), color filters |
| `controllers.lisp` | Input event handling: `on-click`, `on-hover`, `on-key`, `on-text` |
| `resources.lisp` | Resource loading/caching for images and fonts |
| `canvas.lisp` | Pixel-level drawing surface |
| `entities.lisp` | `defentity` macro for reusable drawable objects with input handling |

### Drawing Model

Procedural/non-retained: drawing functions are called directly in the sketch body, which re-executes every frame. No scene graph - but internally, geometry is batched into vertex buffers before rendering (modern OpenGL with VAOs/VBOs). Example:

```lisp
(defsketch my-sketch
    ((x 0))
  (incf x)
  (circle x 200 50))
```

### Input Handling

Define methods on your sketch class:
```lisp
(defmethod on-click ((instance my-sketch) x y) ...)
(defmethod on-key ((instance my-sketch) key state) ...)
(defmethod on-hover ((instance my-sketch) x y) ...)
```

### Key Patterns

- Slot bindings in `defsketch` are available as lexical variables in the body
- `setup` generic function runs once on sketch start/restart
- `(copy-pixels t)` prevents screen clearing between frames
- `y-axis` option (`:down` default, or `:up`) controls coordinate system orientation
- Resources are auto-cached; call `load-resource` in body without performance penalty

## Foreign Dependencies

SDL2, SDL2_IMAGE, SDL2_TTF, libffi, OpenGL 3.3+

## Supported Lisp Implementations

SBCL (Linux), CCL (macOS recommended due to GUI threading). See README.org for platform-specific notes.
