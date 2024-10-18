; CSC 151-NN (TERM)
; Lab: Decomposition 
; Authors: YOUR NAMES HERE
; Date: THE DATE HERE
; Acknowledgements: 
;   ACKNOWLEDGEMENTS HERE

; +-----------+------------------------------------------------------
; | Libraries |
; +-----------+

(import image)

; +-------------------------+----------------------------------------
; | Exercise 0: Preparation |
; +-------------------------+

; First, introduce yourself to your partner.  You may want to discuss any
; questions you had on the readings and/or review any additional comments on the
; readings.

; --- ...now partner A drives! ---

; +---------------------+--------------------------------------------
; | Exercise 1: A party |
; +---------------------+

; As the A above suggests, Partner A should drive on this exercise and
; Partner B should navigate.  If all goes well, this exercise should 
; take you about ten minutes.

; Define an image called `party-people` that looks like the image in
; the lab instructions.

; To draw equilateral triangles, you should use the `triangle` function, 
; e.g.,

; ; Creates an equilateral triangle with sides of length
; ; 10 (in pixels) that is solid and green.
; (define little-green-triangle
;   (triangle 10 "solid" "green"))

; As the reading suggests, you should use the technique of *algorithmic
; decomposition* to break down the image into smaller parts that are
; simpler to implement.  Your code should use `define` commands to
; explicitly name these smaller parts so that your code reflects that
; decomposition you identified.

(define hat 
(triangle 10 "solid" "purple"))

(define head
(circle 20 "outline" "black"))

(define body
(rectangle 2 40 "solid" "black"))

(define party-person
  (above 
    hat
    head
    body
  )
)

(define party-people
  (beside
    party-person
    party-person
    party-person
    party-person
    party-person
  ))
party-people

; Remember that `define` will define a name for your drawing. For
; the drawing to be produced, you need to evaluate it! Uncomment
; the following reference to your newly defined drawing to see it!

; party-people

; --- ...now partner B drives! ---

; +-------------------------+----------------------------------------
; | Exercise 2: A landscape |
; +-------------------------+

; Now, switch roles!  The navigator of the previous problem should
; become the driver.

; Now, try building a program that defines `landscape` as the image
; that appears in the lab handout.  (Two trees, a cottage, and two
; more trees.)

; Once again, use algorithmic decomposition to break down this image
; into smaller parts, using `define`s to explicitly name the smaller
; parts you identified.

(define leaves (triangle 30 "solid" "green"))
(define trunk (rectangle 10 50 "solid" "brown"))
(define tree 
  (above leaves trunk))
  
(define roof (triangle 50 "solid" "blue"))
(define walls (rectangle 50 50 "solid" "red"))
(define house (above roof walls))

(define landscape
  (beside tree tree house tree tree))
landscape

; TODO: uncomment once you have begun to fill in the definition above!

; landscape

; --- ...now partner A drives! ---

; +------------------------------+-----------------------------------
; | Exercise 3: Falling dominoes |
; +------------------------------+

; Switch roles yet again!  This exercise is another one that should
; take fifteen minutes or so.

; For this exercise, we'll introduce two new drawing functions:
;
; * `(color r g b a)` produces a string suitable for passing as a color
;   to any one of the drawing functions. The arguments are numbers
;   specifying a color in rgba form.
; * `(overlay/offset img1 dx dy img2)` places the images `img1` and
;   `img2`, on top of each other but with img2 offset by `dx` and `dy`
;   pixels relative to `img1.

; Use these functions to define an image, `the-planets`, that
; looks like the one in the lab handout.

; We recommend first *experimenting* with overlay/offset to get the
; effect of the *drop shadow* in the image. You can also look online
; for a "rgb color picker" that will help you figure out the right
; components for a given color you might have in mind. Your image
; does not need to match exactly, but do your best!

; Again, use functional decomposition and name the components of your
; image appropriately with `define`s.

(import image)

(define sun 
  (overlay/offset (circle 250 "solid" "orange") 5 5 (circle 251.5 "solid" "black"))
)

(define space 
(rectangle 20 2 "solid" "white"))

(define Mercury
(overlay/offset (circle 20 "solid" "grey") 1 1 (circle 20.2 "solid" "black")))

(define Venus
(overlay/offset (circle 40 "solid" "yellow") 1.3 1.3 (circle 41 "solid" "black")))

(define Earth
(overlay/offset (circle 50 "solid" "blue") 1.5 1.5 (circle 50.4 "solid" "black")))

(define Mars
(overlay/offset (circle 45 "solid" "red") 1.49 1.49 (circle 45.4 "solid" "black")))

(define Jupiter
(overlay/offset (circle 100 "solid" "brown") 3 3 (circle 101.4 "solid" "black")))

(define Saturn
(overlay/offset (circle 90 "solid" (color 200 250 240 1)) 2.7 2.7 (circle 90.4 "solid" "black")))

(define Uranus
(overlay/offset (circle 75 "solid" (color 0 250 240 1)) 2.5 2.5 (circle 76.4 "solid" "black")))

(define Neptune
(overlay/offset (circle 70 "solid" (color 0 125 120 1)) 2.4 2.4 (circle 70.4 "solid" "black")))

(define planets
(beside 
  sun
    space
  Mercury
    space
  Venus
    space
  Earth
    space
  Mars
    space
  Jupiter
    space
  Saturn
    space
  Uranus
    space
  Neptune)
)
planets
; TODO: uncomment once you have begun to fill in the definition above!

; the planets

; --- ...now partner B drives! ---

; +---------------------------+-----------------------------------------
; | Exercise 4: Sun triangles |
; +---------------------------+

; Switch roles one last time!  This is perhaps the longest of the
; exercises. Even if you don't complete this exercise, you should read the
; material below, put in your best effort, and capture your work below.

; You will find the standard `overlay` function useful for this exercise:

; `(overlay img1 img2 ...)` creates a new image that appears as if
; the images were overlaid on top of each other (not above, but
; "nearer" in the third dimension. This requires that we change the
; *opacity* of the shape's color.

; In addition, when we specify the color of an image with the
; `(color r g b a)` function, we can also fill of an image, *e.g.*, using
; `"solid"` as in `(square 50 "solid" "red")`, we can also specify a
; number, *e.g.*, `(square 50  "red")`.  The number, which must
; range from 0–255 is interpreted as the degree of *opacity* of
; the shape's color.  0 is interpreted as fully transparent whereas
; 255 is equivalent to `"solid"`.

; Use these functions to define an image, `sun-triangles`, that looks
; like the one in the lab handout.

; The image is a collection of transparent yellow triangles with black
; outlines on top of an orange circle.

; This one is a bit trickier than the previous ones, and there is a
; few ways you might approach it.  Not all approaches will result in
; the exact same image, so we aren't looking for your result to be
; identical, but it is possible, and we encourage you to think carefully
; about your decomposition to get that result.

; At the very least, you will need to draw yellow triangles with
; outlines.  To achieve this effect, you should use `overlay` and two
; triangles with a combination of `"solid"` and `"outline"` fills.

(define sun (circle 100 "solid" "orange"))

(define triangle_1 (triangle 200 "solid" (color 255 255 0 0.5)))
(define outline_triangle_1 (triangle 200 "outline" "black"))


(define triangle_2 (rotate 180 (triangle 200 "solid" (color 255 255 0 0.5))))
(define outline_triangle_2 (rotate 180 (triangle 200 "outline" "black")))

(define sun-triangles
  (overlay triangle_1 outline_triangle_1 triangle_2 outline_triangle_2 sun))
sun-triangles

; +---------------------+--------------------------------------------
; | Submitting your lab |
; +---------------------+

; Yay!  You're done!  Well, you are almost done.  You still need
; to submit the lab and make sure it passes the autograder.

; Once you have all the exercises in your file, `decomposition.scm`,
; it is now complete!  Finally, **_one_ member of your group** can
; upload the completed `decomposition.scm` file to Gradescope for
; this lab.  Make sure that you upload you work as a group assignment
; and that you include your partner in the submission!  

; Don't forget to check the autograder results.  Note that the
; autograder gives 0.999 rather than 1 if you get everything 
; correct.

; If you get errors from the autograder, you should discuss them with
; one of the class staff (or you can try to resolve them yourself).

; You should also check to make sure that the file looks readable.
; (It usually does, but there are times that Scheme does strange
; things to your file.  In such cases, ask the class staff for help.)

; Finally, we would also recommend that you use Teams or email to
; exchange files or portions thereof.

; +---------------------------+--------------------------------------
; | For those with extra time |
; +---------------------------+

; If you find that you have extra time, you should attempt the 
; following exercises.  You need not turn them in.

; --- ...either partner can drive for this one! ---

; +------------------------------------+-----------------------------
; | Extra: Expanding prior exercises   |
; +------------------------------------+

; Expand one of the ideas from a prior exercise.  Perhaps you'll add
; a door to the house, or some fruit to the trees, or bowties to the
; party people. You can even try to use transparency and overlay/offset
; to add texture to the planets. Have fun!
