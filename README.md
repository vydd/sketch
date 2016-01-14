# Sketch

Sketch is a Common Lisp interpretation of [Processing Language](https://processing.org). Saying it is an interpretation is important, because Sketch doesn't strive to implement Processing API in Common Lisp, but rather reimplement the idea behind Processing: to build an easy to use environment for the creation of electronic art, visual design, game prototyping, game making, computer graphics, exploration of human-computer interaction, and more.

![Examples](http://i.imgur.com/MNZUwz8.png)

## Installation

Today (January 2016), [Quicklisp](https://www.quicklisp.org/beta/) is Common Lisp's de facto package manager. Sketch is not a part of Quicklisp yet, but it is intended to be used with it, so you will have to clone it to your local-projects directory manually. If you're not sure how to do that, read the [Quicklisp FAQ](https://www.quicklisp.org/beta/faq.html).

### Requirements

#### Common Lisp Implementation

Sketch should be compatible with all major Common Lisp implementations and all major operating systems - more specifically, all CL implementations and operating systems that [cl-sdl2](https://github.com/lispgames/cl-sdl2) runs on. Incompatibily with any of those is considered a bug.

Sketch is known to work with:

* CCL 1.11 on Mac OS X
* CCL SVN 1.12.dev.r16617 on Arch Linux
* SBCL on Debian Unstable
* SBCL 1.2.16 on Arch Linux

Sketch is known to *not* work with:

* SBCL 1.2.15 on Mac OS X
  _Sketch can't handle input and the window's titlebar is black. These kinds of issues are a known problem with Mac OS X, because it needs its GUI threads to be main, and CCL is the only lisp that accounts for that out of the box. There are ways to counter this, but until a solution finds its way into this repository, SBCL on Mac OS X will stay on this list. In the meantime, use CCL._

If you test it on other systems, please send a pull request to include your results.

#### Foreign depenedencies

##### SDL2

SDL2 is Sketch's only backend. It is a C library which you will need to download manually from [libsdl webpage](https://www.libsdl.org/download-2.0.php). Select the relase compatible with your operating system, or compile from the source code.

##### SDL2 Image & SDL2 TTF
For loading image and font files, Sketch relies on SDL2 Image and SDL2 TTF, respectively, both part of the SDL project.

##### libffi

Some users have reported that [libffi](https://sourceware.org/libffi/) needed to be installed to make Sketch work.

##### OpenGL

Sketch requires graphics hardware and drivers with support for GL version 3.3.

#### Common Lisp Systems

Not all CL systems (or system version) that Sketch uses can be found in Quicklisp. These systems will need to be cloned to the local-projects directory in addition to Sketch itself:

https://github.com/vydd/sketch
https://github.com/vydd/cl-geometry
https://github.com/lispgames/glkit
https://github.com/lispgames/mathkit
https://github.com/lispgames/cl-sdl2-image
https://github.com/Failproofshark/cl-sdl2-ttf
https://github.com/lispgames/sdl2kit
https://github.com/lispgames/cl-sdl2

#### Running Sketch

After fetching all dependencies, fire up your favorite Common Lisp implementation and type `(ql:quickload :sketch)`.

### Running provided examples

To get a feel for what Sketch can do, and also to make sure that everything has been installed correctly, you can look at the examples. The code below will run all four currently provided examples at once. Note that on older machines running three sketches at once might result in a small degradation in performance, so you might want to run sketches separately.

```lisp
CL-USER> (ql:quickload :sketch-examples)
CL-USER> (make-instance 'sketch-examples:hello-world)
CL-USER> (make-instance 'sketch-examples:sinewave)
CL-USER> (make-instance 'sketch-examples:brownian-turtle)
CL-USER> (make-instance 'sketch-examples:life) ; Click to toggle cells,
	                                           ; any key to toggle iteration
```

### Running example code from this page

In this, and all other examples, we're going to assume that Sketch is loaded with `(ql:quickload :sketch)`, and that we're in package `:SKETCH`, either after explicitly doing that via REPL:

```lisp
CL-USER> (ql:quickload :sketch)
...
CL-USER> (in-package :sketch)
...
SKETCH> _

```

or by depending on `:SKETCH` and `:USE`ing it in a system and package definition respectively, like it's done for `:SKETCH-EXAMPLES`.

## Hello, World

Defining sketches is done via the `DEFSKETCH` macro, that wraps `DEFCLASS`. Using `DEFCLASS` is still possible, but `DEFSKETCH` makes everything so much easier, and in these examples, we're going to pretend that's the only way.

```lisp
;; Let's say it one more time: All examples on this page need to have
;; :sketch loaded, and all of them execute inside the :sketch package.

(defsketch hello-world () ())
(make-instance 'hello-world)
```

If all goes well, this should give you an unremarkable gray window.

### Color

Let's add some color. Assuming that you're using Emacs + SLIME, or a similarly capable environment, you can just re-evaluate with the following code:

```lisp
(defsketch hello-world () ()
  (background +yellow+))
```

The screen becomes yellow. There are a couple of things to note. Drawing code doesn't need to go into a special function or method, or be binded to a sketch explicitly. `DEFSKETCH` is defined as `(defsketch sketch-name window-options slot-bindings &body body)`: that body is your drawing code. We will get to `WINDOW-OPTIONS` and `SLOT-BINDINGS` later. The other thing is that Sketch comes with its own color library. Currently, it supports RGBA and HSBA models, multiple convenient ways of defining colors, basic color mixing, basic color filters and a small number of predefined colors.

### Drawing

Let's draw something.

```lisp
(defsketch hello-world () ()
  (background (rgb 1 1 0))
    (with-pen (make-pen :stroke (gray 0) :fill (rgb-255 200 200 200))
      (ngon 6 (/ width 2) (/ height 2) 30 50 :angle 90)))
```

You get a vertically elongated hexagon. Let's see what we said here. We used a `WITH-PEN` block to declare the `PEN` - a set of drawing parameters - applied to elements inside. Notice a couple more ways of declaring colors.

Then we draw a shape, an `NGON`, which is an n-sided convex polygon, inscribed in an ellipse.

### Sketch's usage of CLOS

The sketch is using `WIDTH` and `HEIGHT` to calculate a hexagon's center. These are actually the slots inherited from the `SKETCH` class, made easy to use when drawing by being automatically wrapped inside `WITH-SLOTS` with all slots listed. Other inherited slots are `FRAMERATE`, used for setting hard frame rate limits, useful for debugging, but also as a replacement for more complicated timing code. It defaults to `:AUTO`. Finally, there's `TITLE`, defaulting to `"Sketch"`, and being displayed on the titlebar.

All of these slots can be set using `DEFSKETCH` syntax, using `WINDOW-OPTIONS`:

```lisp
(defsketch using-win-opts (:title "Hello, World"
                           :width 300
                           :height 300
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

#### It's still Common Lisp

You have all the power of CL at your disposal when drawing, and you're not restricted to using only drawing primitives.

```lisp
(defsketch hello-world (:title "Hello, World") ()
  (background (rgb 1 1 0))
    (with-pen (make-pen :stroke (gray 0) :fill (rgb-255 200 200 200))
    (dotimes (i 11)
      (ngon 6 (/ width 2) (/ height 2) (- 30 (* i 3)) (- 50 (* i 3))
            :angle (- 90 (* i 4))))))
```

# WORK IN PROGRESS

## Outro

For everything else, read the code or ask vydd at #lispgames.

Go make something pretty.
