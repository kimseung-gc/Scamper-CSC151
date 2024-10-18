; spaceship.scm
;
; An amazing image of a rainbow-spaceship that I've created.
;
; I have modified my buffers so that the spaceship has a correct
; shape now. Also, I have made the code more concise by reusing 
; the previous columns to define the new column.
;
; CSC 151 (22fa)
; Mini Project 1, Part 1
; Author: Seunghyeon Kim
; Date: 2022-09-21
; Acknowledgements: professor Autry
;rainbow-spaceship
; cleared version
(import image)
(define col1; red
    (rectangle 100 25 "solid" "red")
)
(define col2; red and orange column
    (above 
    col1
    (rectangle 100 25 "solid" "orange")
    )
)
(define col3; col2 and yellow column
    (above 
    col2 
    (rectangle 100 25 "solid" "yellow")
    )
)
(define col4; col3 and green column
    (above 
    col3
    (rectangle 100 25 "solid" "green")
    )
)
(define col5; col4 and blue column
    (above 
    col4
    (rectangle 100 25 "solid" "blue")
    )
)
(define col6; col5 and violet column
    (above 
    col5
    (rectangle 100 25 "solid" "violet")
    )
)
(define rainbow-spaceship
    (beside
        col1
        col2
        col3
        col4
        col5
        col6; some columns are repetitive which indicates that they can be reused.
        col5
        col4
        col3
        col2
        col1
    )
)
rainbow-spaceship
;I think that the pattern could be seen by the fact that the column index increases from 1~6, but then decreases from 6~1.
;Due to the same reason, symmetry of the columns could be seen.