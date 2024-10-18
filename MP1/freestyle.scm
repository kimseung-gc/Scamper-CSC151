(import image)
; nothing changed

; freestyle.scm
; An amazing image of a fire-extinguisher that I've created.
; CSC 151 (22fa)
; Mini Project 1, Part 2
; Author: Seunghyeon Kim
; Date: 2022-09-04
; Acknowledgements: professor Autry, Siho Kim
(define ellipse-width 100); this is the standard of the size of the fire extinguisher image that most of the other variables will be using.
(define ellipse-length 50); same as above
(define body-scalage 1.1); scalage of body:base. Should remain unchanged for most of the circumstances.
(define head-scalage 0.9); scalage of head:base. Should remain unchanged for most of the circumstances.
(define background (rectangle 500 500 "solid" "white")); Used for debugging purposes when overlaying functions are used.
(define body-part1
    (overlay/offset 
        (rectangle (* body-scalage ellipse-width) 200 "solid" (color 200 0 0 1)) 
        0 
        172
        (ellipse (* body-scalage ellipse-width) (* body-scalage ellipse-length) "solid" (color 200 0 0 1))
        )
)
(define body-part2-sub1; sub part of the body-part2 which is required for clearence of the code.
    (overlay/offset
        (rectangle 70 20 "solid" (color 200 0 0 1))
        50
        0
        (circle 20 "solid" (color 200 0 0 1))
    )
)
(define body-part2; assembly of the sub part of the body-part2-sub1 and the other solid
    (overlay/offset
        (circle 20 "solid" (color 200 0 0 1))
        20 
        0
        body-part2-sub1
    )
)
(define head-part1-top; top part of head-part1. It needs an outline to make same colored edges distinguishable.
    (overlay
        (ellipse (* (* ellipse-width 0.95) head-scalage) (* (* ellipse-length 0.95) head-scalage) "outline" "black")
        (ellipse (* ellipse-width head-scalage) (* ellipse-length head-scalage) "outline" "black")
        (ellipse (* ellipse-width head-scalage) (* ellipse-length head-scalage) "solid" (color 200 0 0 1))
    )
)
(define head-part1-bottom; top part of head-part1. It needs an outline to make same colored edges distinguishable.
    (overlay
        (ellipse (* ellipse-width head-scalage) (* ellipse-length head-scalage) "outline" "black")
        (ellipse (* ellipse-width head-scalage) (* ellipse-length head-scalage) "solid" (color 200 0 0 1))
    )
)
(define head-part1
    (overlay/offset
        (rectangle 90 22.5 "solid" (color 200 0 0 1))
        0
        0
        head-part1-bottom
    )
)
(define label-up
    (overlay/offset
        (ellipse 90 45 "solid" "white")
        0
        -25
        (rectangle 90 50 "solid" "white")
    )
)
(define body; this is the trunk of the fire extinguisher that can be expressed with several squares and circles.
    (overlay/offset
        body-part1
        0
        -20
        body-part2
    )
)
(define head 
    (overlay/offset
        head-part1-top
        0
        20
        head-part1
    )
)
(define base; this is the very bottom of the fire extinguisher. It should be darker red due to the shadow nature.
    (overlay 
        (ellipse ellipse-width ellipse-length "outline" "black") 
        (ellipse ellipse-width ellipse-length "solid" (color 139 0 0 1))
        )
)
(define assembly
    (overlay/offset 
        head 
        -10 
        30 
        (overlay/offset body 5 202.5 base))
)
(define labelling
    (overlay/offset
        (ellipse 90 45 "solid" (color 200 0 0 1))
        0
        20
        label-up)
)
(define assembly2
    (overlay/offset
        labelling
        -10
        -100
        assembly
    )
)
(define my-image
    (overlay
        assembly2
        background
    )
)
my-image