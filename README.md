# Sketch

Sketch is a Common Lisp interpretation of [Processing Language](https://processing.org). Saying it is an interpretation is important, because Sketch doesn't strive to implement Processing API in Common Lisp, but rather reimplement the very idea behind Processing: to build an easy to use environment for creation of electronic art, visual design, game prototyping, computer graphics; exploration of human-computer interaction and more.

In its current form, it is not intended to be used as a game engine, although simple games can be created in Sketch. This is because Sketch is in its infancy, and designing it to be intuitive and easy to use is much more important than optimizing it for speed - which is important for serious game development.

![Examples](http://i.imgur.com/MNZUwz8.png)

## Installation

Today (November 2015), [Quicklisp](https://www.quicklisp.org/beta/) is Common Lisp's de facto package manager. Sketch is not part of Quicklisp yet, but it is intended to be used with it, so you will have to clone it to your local-projects directory manually. If you're not sure how to do that, read the [Quicklisp FAQ](https://www.quicklisp.org/beta/faq.html).

SDL2 is Sketch's only backend. It is a C library which you will need to download manually from [libsdl webpage](https://www.libsdl.org/download-2.0.php). Select the relase compatible with your operating system, or compile from the source code.

### Requirements

Sketch should be compatible with all major Common Lisp implementations and all major operating systems - more specifically, all CL implementations and operating systems that [cl-sdl2](https://github.com/lispgames/cl-sdl2) runs on. Incompatibily with any of those is considered a bug.

Sketch is known to work with:

* CCL 1.10 on Mac OS X
* SBCL on Debian Unstable
* SBCL 1.2.16 on Arch Linux

Sketch is known to *not* work with:

* SBCL 1.2.15 on Mac OS X
  _Sketch can't handle input and the window's titlebar is black. These kind of issues are a known problem with Mac OS X, because it needs its GUI threads to be main, and CCL is the only lisp that accounts for that out of the box. There are ways to counter this, but until a solution finds its way into this repository, SBCL on Mac OS X will stay on this list. In the meantime, use CCL._

If you test it on other systems, please send a pull request to include your results.

### Running provided examples

To get a feel for what Sketch can do, you can look at the examples. The code below will run all three currently provided examples at once. Note that on older machines running three sketches at once might result in small degradation in performance, so you might want to run sketches separately.

```lisp
CL-USER> (ql:quickload :sketch-examples)
CL-USER> (make-instance 'sketch-examples:sinewave)
CL-USER> (make-instance 'sketch-examples:brownian-turtle)
CL-USER> (make-instance 'sketch-examples:life) ; Click to toggle cells,
	                                           ; any key to toggle iteration
```

### Running example code from this page

In this, and all other examples, we're going to assume that Sketch is loaded with `(ql:quickload :sketch)`, and that we're in package `:SKETCH`, either after explicitely doing that via REPL:

```lisp
CL-USER> (ql:quickload :sketch)
...
CL-USER> (in-package :sketch)
...
SKETCH> _

```

or by depending on `:SKETCH` and `:USE`ing it in system and package definition respectively, like it's done for `:SKETCH-EXAMPLES`.

## Hello, World

Defining sketches is done via `DEFSKETCH` macro, that wraps `DEFCLASS`. Using `DEFCLASS` is still possible, but `DEFSKETCH` makes everything so much easier, and in these examples, we're going to pretend that's the only way.

```lisp
;; Let's say it one more time: All examples on this page need to have
;; :sketch loaded, and all of them execute inside :sketch package.

(defsketch hello-world () ())
(make-instance 'hello-world)
```

If all goes well, this should give you an unremarkable green window. Green indicates that everything is up and running, but that you haven't done any drawing yet.

### Color

What if you like yellow more? Assuming that you're using Emacs + SLIME, or similarly capable environment, you can just re-evaluate with the following code.

```lisp
(defsketch hello-world () ()
  (background (rgb 1 1 0)))
```

The screen becomes yellow. There are a couple of things to note. Drawing code doesn't need to go into a special function or method, or be binded to a sketch explicitly. `DEFSKETCH` is defined as `(defsketch sketch-name window-options slot-bindings &body body)`: that body is your drawing code. We will get to `WINDOW-OPTIONS` and `SLOT-BINDINGS` later. The other thing is that Sketch comes with its own color library. Currently, it supports RGBA and HSBA models, multiple convenient ways of defining colors and basic color mixing. Soon to come are palles, color harmonies and predefined colors.

### Drawing

Let's draw something.

```lisp
(defsketch hello-world () ()
  (background (rgb 1 1 0))
    (with-pen (make-pen :stroke (gray 0) :fill (rgb-255 200 200 200))
      (ngon 6 (/ width 2) (/ height 2) 30 50 :angle 90)))
```

You get a vertically elongated hexagon. _NOTE: Lines will probably look rough. That's expected right now. Soon, line drawing will get revamped and your lines will be much more beautiful._ Let's see what we said here. We used a `WITH-PEN` block to declare the `PEN` - a set of drawing parameters - applied to elements inside. At the time of writing this, only `:FILL` and `:STROKE` colors are supported, and we're making use of both. Notice a couple more ways of declaring colors. Then we draw a shape, an `NGON`, which is an n-sided convex polygon, inscribed in an ellipse.

### Sketch's usage of CLOS

The sketch is using `WIDTH` and `HEIGHT` to calculate hexagon's center. These are actually the slots inherited from `SKETCH` class, made easy to use when drawing by being automatically wrapped inside `WITH-SLOTS` with all slots listed. Other inherited slots are `FRAMERATE`, used for setting hard frame rate limits, useful for debugging, but also as a replacement for more complicated timing code. It defaults to `:AUTO`. Next, there's `COPY-PIXELS`, that defults to `NIL`. It controls the way the drawing works. If it's enabled, drawing is incremental; it's "copying the pixels" from frame to frame. Finally, there's `TITLE`, defaulting to `"Sketch"`, and being displayed on the titlebar.

All of these slots can be set using `DEFSKETCH` syntax, using `WINDOW-OPTIONS`:

```lisp
(defsketch using-win-opts (:title "Hello, World"
                           :width 300
                           :height 300
						   :copy-pixels nil
						   :framerate :auto)
  ())
```

The third argument, `SLOT-BINDINGS`, is reserved for defining slots. It's different from standard slot definition, looking more like a let init-form. Currently, initial values are mandatory and there are no special slot modifiers, but the latter will change soon.

```lisp
(defsketch full-utilization (:title "We have slots")
    ((first-slot 0)
     (foobar (if t "Foo?" "???")
   (sym 'DEFAULT-SYMBOL)))
   (background (hsb 0.5 1.0 1.0)))
```

These user-defined slots are also wrapped inside `WITH-SLOTS`.

### It still is Common Lisp

You have all the power of CL at disposal when drawing, and you're not restricted to using only drawing primitives.

```lisp
(defsketch hello-world (:title "Hello, World") ()
  (background (rgb 1 1 0))
    (with-pen (make-pen :stroke (gray 0) :fill (rgb-255 200 200 200))
    (dotimes (i 11)
      (ngon 6 (/ width 2) (/ height 2) (- 30 (* i 3)) (- 50 (* i 3))
            :angle (- 90 (* i 4))))))
```

### Animation

```lisp
(defsketch hello-world (:title "Hello, World" :height 300 :width 300)
    ((i 0)
     (pen (make-pen :stroke (gray 0) :fill (rgb-255 200 200 200))))
  (background (rgb 1 1 0))
  (setf i (mod (1+ i) 360))
  (with-pen pen
    (ngon 6 (/ width 2)
	  (/ height 2)
	  (- 80 (* (sin (radians i)) 33))
	  (- 80 (* (sin (radians i)) 33))
	  :angle (- 90 (* (sin (radians i)) 44)))))
```
# WORK IN PROGRESS

## TODO

- [ ] Additional shapes
- [ ] More color utilities (palletes, color harmony...)
- [ ] Stroke options and better line drawing
- [ ] Image loading
- [ ] Typography
- [ ] Controllers
- [ ] Simple audio
- [ ] Basic physics
- [ ] Basic 3D

## Outro

For everything else, read the code or ask vydd at #lispgames. 

Go make something pretty.
