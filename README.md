# Sketch

## What is Sketch?

Sketch is a Common Lisp interpretation of [Processing Language](https://processing.org). It doesn't strive for 100% compatibility, but rather to being similar in spirit by providing an easy interface to computer graphics and human-computer interaction.

## What can I do with it?

Sketch is in its infancy, but colors and basic shapes are mostly done. Being based on [sdl2kit](https://github.com/lispgames/sdl2kit) means that you get windowing and event handling for free.

For start, there will be no 3D in Sketch, but the plan is to add basic support it later.

## Alright, how do I work with color?

Color is implemented as a struct, containing both RGB and HSB values, plus ALPHA. There're many constructors implemented for convenience - `RGB`, `HSB`, `GRAY` using normalized values and `RGB-255`, `HSB-360` `GRAY-255` using values in [0, 255] range for rgb and gray, and using degrees for hue, along with percentages for saturation and brightness.

There's also a `HEX-TO-COLOR` constructor, which builds colors from hex strings, like `"ff00ff"`, `"cdc"`, `"abcdef99"` or `"abcd"`.

You can lerp two colors using "`LERP-COLOR`", and choose to do it either in RGB or HSB space.

## How do I get colors on screen?

If you want to color the window, use `(background your-color)`.

If you want your shapes to be colorful, use a `PEN`. A `PEN` is a struct that has its fill and stroke (both colors). A pen is created using standard `MAKE-PEN` constructor. To use a pen with shapes, wrap them inside `WITH-PEN` macro.

## What shapes?

`ELLIPSE`, `LINE`, `POINT`, `QUAD`, `RECT`, `NGON` (regular convex polygon) and `TRIANGLE`.

# Ok, show me.

There's a couple of examples included, and we're going to dissect the second one (it being much more interesting.

So, firstly, you'll need to create a class inheriting from `SKETCH:SKETCH`.

```
(defclass sketch-example-2 (sketch)
  ((title :initform "Sketch example - 2")
   (width :initform 400)
   (height :initform 400)
   (framerate :initform 60)
   (steps :initform 0)
   (xs :initform 40)
   (pen :initform (make-pen :fill (gray 1.0)))))
```

If you choose to, you can set title, width, height and framerate there. Of course, you're free to add anything else you need.

There are two generic methods you can implement, `SKETCH:SETUP` and `SKETCH:DRAW`. `SETUP` will be called once before creating the window, and `DRAW` will get called to draw each frame - this is where your drawing code will live. For this example, we won't use setup.

```
(defmethod draw ((s sketch-example-2))
  (with-slots (steps xs pen width height) s
    (incf steps)
    (background (gray 0.2))
    (with-pen pen
      (mapcar (lambda (x)
		(ellipse (* x (/ width xs))
			 (+ (/ height 2)
			    (* (/ height 4)
			       (sin (* TWO_PI (/ (+ (/ steps 4) x) xs)))))
			 (/ width xs 3)
			 (/ width xs 3)))
			 (alexandria:iota xs)))))
```

That's all. You can start this sketch with `(make-instance 'sketch-example-2)`.

![Sketch example 2](http://idle.rs/~vydd/sketch/sketchexample2.png)
This being lisp, you can change the code in `DRAW` while it executes and see results immediately.

## Outro

For everything else, read the code or ask vydd at #lispgames. 

Go make something pretty.

# TODO

* Complete shapes
* Image loading
* Typography
* Simple audio
* Basic physics
* Basic 3D
