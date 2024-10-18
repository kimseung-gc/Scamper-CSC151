(import image)
(import music)

;; The original mis-spelling of the name erhenstein was changed to ehrenstein.
;; Added more test cases for grid. I have added test cases for all the functions
;; , but the "test-case" cannot compare image composition or music composition,
;; so it is quite useless. I indented them since it does not really compare the
;; two answers...

;; CSC-151-03 (Fall 2022)
;; Mini-Project 4: Repetitive Music and Images
;; Seunghyeon Kim
;; 2022-09-30
;; ACKNOWLEDGEMENTS:
;;      Marty Stepp and Stuart Reges from the University of Washington

; (ehrenstein length n box-color circ-color outline-color): image composition?
; length: number?(> 0)
; n: integer? (>= 0)
; box-color: string?(string that describes the color or made by the "color" function)
; circ-color: string?(string that describes the color or made by the "color" function)
; outline-color: string?(string that describes the color or made by the "color" function)
; It produces the ehrenstein image with box-colored box with length pixels, n circles with 
; circ-color at the base and outline-color circular and square outlines on top. 
(define ehrenstein 
    (lambda (length n box-color circ-color outline-color)
        (let*
            (
                [r0
                    (/ length 2)
                ]
                [sizeL
                    (map (lambda (x) (* (/ x n) r0)) (range 1 (+ n 1)))
                ]
                [compL
                    (map (lambda (size) (circle size "outline" outline-color)) sizeL)
                ]
                [outline_comp
                    (apply overlay compL)
                ]
                [initCirc
                    (circle r0 "solid" circ-color)
                ]
                [circleComp
                    (overlay outline_comp initCirc)
                ]
                [squareOutline
                    (path length length (list (pair 0 (/ length 2)) (pair (/ length 2) length) (pair length (/ length 2)) (pair (/ length 2) 0) (pair 0 (/ length 2))) "outline" outline-color)
                ]
                [Circ+Sq
                    (overlay squareOutline circleComp)
                ]
                [bkgrndSqr
                    (square length "solid" box-color)
                ]
                [Fullcomposition
                    (overlay Circ+Sq bkgrndSqr)
                ]
            )
            Fullcomposition
        )
    )
)
; (grid m n img): image composition?
; m: integer? (> 0)
; n: integer? (> 0)
; img: image composition?
; It produces a mxn grid with repeated img.
(define grid
    (lambda (m n img)
        (let*
            (
                [row1 (apply beside (make-list n img))]
                [composition (apply above (make-list m row1))]
            )
            composition
        )
    )
)
; (test-case "2x2 red circle grid" equal? (beside (above (circle 15 "solid" "red") (circle 15 "solid" "red")) (above (circle 15 "solid" "red") (circle 15 "solid" "red"))) (grid 2 2 (circle 15 "solid" "red")))
; For some reason the test case above does not work as intended probably due to the data type of the output. In other words,
; the "test-case" function cannot compare two images. Not only images, but I also notice that it does not work for music 
; library also...
(define ehrenstein-1
    (ehrenstein 200 5 "red" "yellow" "black")
)
(define ehrenstein-2
    (ehrenstein 100 10 "aqua" "orange" "black")
)
(define ehrenstein-3
    (ehrenstein 50 0 "white" "white" "green")
)
(define ehrenstein-4
    (grid 3 3 (ehrenstein 100 10 "red" "yellow" "orange"))
)
(define ehrenstein-5
    (grid 3 2 (ehrenstein 50 5 "blue" "green" "white"))
)
; (chord root intervals dr): composition?
; root: integer? (between 0 to 120)
; intervals: string list?
; dr: duration?
; creates a chord, a collection of notes specified by the list intervals relative 
; to root note root for the given duration. The root is specified as a MIDI note 
; value, and the intervals are given as a list of strings of short interval names 
; as described on Wikipedia.
(define chord
    (lambda (root intervals dr); (list "P1" "m3" "P5")
        (let*
            (
                ; (ntID str): note?
                ; str: string? (only intervals)
                ; It is a helper function of the function chord that takes in str as an interval
                ; and return the resultant note of applying the intervals to the specified root
                ; with the notes.
                [ntID 
                    (lambda (str) 
                        (cond
                            [(string=? str "P1") (note (+ root 0) dr)]
                            [(string=? str "m2") (note (+ root 1) dr)]
                            [(string=? str "M2") (note (+ root 2) dr)]
                            [(string=? str "m3") (note (+ root 3) dr)]
                            [(string=? str "M3") (note (+ root 4) dr)]
                            [(string=? str "P4") (note (+ root 5) dr)]
                            [(string=? str "P5") (note (+ root 7) dr)]
                            [(string=? str "m6") (note (+ root 8) dr)]
                            [(string=? str "M6") (note (+ root 9) dr)]
                            [(string=? str "m7") (note (+ root 10) dr)]
                            [(string=? str "M7") (note (+ root 11) dr)]
                            [(string=? str "P8") (note (+ root 12) dr)]
                        )
                    )
                ]
            )
            (apply par
                (map ntID intervals)
            )
        )
    )
)
; (test-case "chord comparison" equal? (par (note 60 qn) (note 63 qn) (note 67 qn)) (chord 60 (list "P1" "m3" "P5") qn))
; Same as the test case mentioned above, the test-case cannot compare the composition of images
; and the composition of notes which is responsible for the failure of test case above.
; (degree->offset degree)
; degree: integer?
; returns the number of semitones away from the root the given note degree is 
; in the major scale.
(define degree->offset 
    (lambda (degree)
        (let
            (
                [pattern (list 0 2 4 5 7 9 11 12)]
                [quot (quotient (- degree 1) 7)]
                [rem (remainder (- degree 1) 7)]
            )
            (+ (list-ref pattern rem) (* quot 12))
        )
    )
)
(test-case "degree to offset as a numerical value" equal? 7 (degree->offset 5))
(test-case "degree to offset as a numerical value" equal? 16 (degree->offset 10))
(test-case "degree to offset as a numerical value" equal? 0 (degree->offset 1))
; (progression key chords drs): composition?
; key: integer?
; chords: string list? (with the degree of chords in stringed roman numericals. 
; It should be either uppercase or lowercase depending on whether it is a major 
; or minor.)
; drs: duration list? (its length has to equal the length of chords)
; creates a musical progression, a collection of chords (input by the variable 
; chords) played in sequence with the duration of durations.
(define progression
    (lambda (key chords dr)
        (let*
            (
                ; (ChrdID chrdStr): list?
                ; chrdStr: string? (numbers in roman numberals)
                ; it returns the list of degree on the first element, and the
                ; cdr of the list will be the list of intervals depending on
                ; whether it is major or minor.
                [ChrdID
                    (lambda (chrdStr)
                        (cond
                            [(string=? chrdStr "I") (cons 1 (list "P1" "M3" "P5"))]
                            [(string=? chrdStr "II") (cons 2 (list "P1" "M3" "P5"))]
                            [(string=? chrdStr "III") (cons 3 (list "P1" "M3" "P5"))]
                            [(string=? chrdStr "IV") (cons 4 (list "P1" "M3" "P5"))]
                            [(string=? chrdStr "V") (cons 5 (list "P1" "M3" "P5"))]
                            [(string=? chrdStr "VI") (cons 6 (list "P1" "M3" "P5"))]
                            [(string=? chrdStr "VII") (cons 7 (list "P1" "M3" "P5"))]
                            [(string=? chrdStr "VIII") (cons 8 (list "P1" "M3" "P5"))]
                            [(string=? chrdStr "i") (cons 1 (list "P1" "m3" "P5"))]
                            [(string=? chrdStr "ii") (cons 2 (list "P1" "m3" "P5"))]
                            [(string=? chrdStr "iii") (cons 3 (list "P1" "m3" "P5"))]
                            [(string=? chrdStr "iv") (cons 4 (list "P1" "m3" "P5"))]
                            [(string=? chrdStr "v") (cons 5 (list "P1" "m3" "P5"))]
                            [(string=? chrdStr "vi") (cons 6 (list "P1" "m3" "P5"))]
                            [(string=? chrdStr "vii") (cons 7 (list "P1" "m3" "P5"))]
                            [(string=? chrdStr "viii") (cons 8 (list "P1" "m3" "P5"))]
                        )
                    )
                ]
                [ChrdL (map ChrdID chords)]
                [CHRImp
                    (lambda (ChrL dr1) 
                        (chord (+ key (degree->offset (car ChrL))) (cdr ChrL) dr1)
                    )
                ]
                [compL (map CHRImp ChrdL dr)]
            )
            (apply seq compL)
        )
    )
)
;(test-case "progression" equal? (progression 64 (list "I" "V" "vi" "IV") (list hn hn hn hn)) 
;    (seq (par (note 64 hn) (note 68 hn) (note 71 hn)) 
;        (par (note 71 hn) (note 75 hn) (note 78 hn)) 
;        (par (note 73 hn) (note 76 hn) (note 80 hn))
;        (par (note 69 hn) (note 73 hn) (note 76 hn))))

; Same problem as above. "test-case" cannot compare composition data type or image data type.
; Thus, the only way to check is to manually hear the song or check the image with bare eyes.
(define I-V-vi-IV (progression 64 (list "I" "V" "vi" "IV") (list hn hn hn hn)))
(define I-IV-V-IV (progression 60 (list "I" "IV" "V" "IV") (list hn hn hn hn)))
(define ii-V-I (progression 60 (list "ii" "V" "I") (list hn hn hn)))
; The Last Goodbye by AKMU in South Korea
(define Last-GoodBye (progression 60 (list "I" "vii" "VII" "i") (list hn hn hn hn)))