;;; I have corrected one of the duration for mary-had-a-little-lamb
;;; basic version, and I have made the code more concise by using let 
;;; function for the mary-had-a-little-lamb advanced version.

;; CSC-151-03 (Fall 2022)
;; Mini-Project 2: Exploring Strings and Sounds
;; Seunghyeon Kim
;; 2022-09-23
;; ACKNOWLEDGEMENTS:
;;    Siho Kim

(import image)
(import music)

;;; part 1
;;; a, b, c)
;;; i)
;;; (slight-trim str)
;;; str: string?
;;; (sta i) If space on both sides...
;;;   remove spaces from the front and back
;;; (sta ii) If space on beginning only...
;;;   remove spaces from the front
;;; (sta iii) If space on end only...
;;;   remove spaces from the end
;;; If input " adadasdadasda"
;;;   Since the input is matching to (sta ii), it will remove space from the front
;;; If input "adadasdadasda "
;;;   Since the input is matching to (sta iii), it will remove space from the end
;;; If input " adadasdadasda "
;;;   Since the input is matching to (sta i), it will remove space from the front and the end
;;; If input is ""
;;;   Since it does not apply to any of the cases, the function would not do anything
;;; If input is " "
;;;   Since it applies to all (sta i), (sta ii), (sta iii), the function might try to evaluate 
;;;   the first case (sta i), but since it cannot remove 2 " ", the code will return ""
;;; If input is not a string 
;;;   Since number cannot be compared with string, it will throw an error at the if statement.

(define slight-trim 
    (lambda (str) 
        (cond
            [(string=? str " ") 
                ""
            ]
            [(and (string=? (substring str 0 1) " ") (string=? (substring str (- (string-length str) 1) (string-length str)) " ")) 
                (substring str 1 (- (string-length str) 1))
            ]
            [(string=? (substring str 0 1) " ") 
                (substring str 1 (string-length str))
            ]
            [(string=? (substring str (- (string-length str) 1) (string-length str)) " ") 
                (substring str 0 (- (string-length str) 1))
            ]
            [else 
                str
            ]
        )
    )
)

(slight-trim "goa ")
;;; ii)
;;; (starts-with? s1 s2)
;;; s1: string?
;;; s2: string?
;;; (sta i) If (string=? (string-ref s1 0) s2)...
;;;   The code will return true
;;; (sta ii) If (not (string=? (string-ref s1 0) s2))...
;;;   The code will return false
;;; If input is "part" and "p",
;;;   Since the index 0 of "part" is equal to "p", it will throw true.
;;; If input is "art" and " ",
;;;   Since the index 0 of "art" is "a", but it is not " ", it will throw false.
;;; If input is 7,
;;;   Since "unsind" 7 is incomparable with "unsind", it will throw an error.

(define starts-with?
    (lambda (s1 s2)
        (string=? (substring s1 0 (string-length s2)) s2)
    )
)

(starts-with? "abc" "ab")

;;; iii)
;;; (ends-with? s1 s2)
;;; s1: string?
;;; s2: string?
;;; (sta i) If (string=? (string-ref s1 (- (length s1) 1)) s2)...
;;;   The code will return true
;;; (sta ii) If (not (string=? (string-ref s1 (- (length s1) 1)) s2))...
;;;   The code will return false
;;; If input is "part" and "p",
;;;   Since the last index of "part" is not equal to "p", it will throw false.
;;; If input is "art" and "t",
;;;   Since the last index of "art" is equal to "t", it will throw true.
;;; If input is 7 and " ",
;;;   Since 7 is incomparable with " ", it will throw an error.

(define ends-with?
    (lambda (s1 s2)
        (string=? (substring s1 (- (string-length s1) (string-length s2)) (string-length s1)) s2)
    )
)

(ends-with? "abc" "bc")

;;; iv)
;;; (all-digits? str)
;;; str: string?
;;; (sta i) If (= (string-length str) (length (filter char-numeric? (string->list str))))...
;;;   The code will return true
;;; (sta ii) If (not (= (string-length str) (length (filter char-numeric? (string->list str)))))...
;;;   The code will return false
;;; If input is "part",
;;;   Since the input is composed only of strings, it will return false
;;; If input is "840819749837981",
;;;   Since all the string is composed of numbers, it will throw true.
;;; If input is 7,
;;;   Since 7 is not a string, so it will throw an error.

(define all-digits? (lambda (str)
        (= (string-length str) (length (filter char-numeric? (string->list str))))
    )
)

(all-digits? "7")

(define naive-mary-had-a-little-lamb
    (seq 
        (note 69 qn)
        (note 67 qn)
        (note 65 qn)
        (note 67 qn)
        (note 69 qn)
        (note 69 qn)
        (note 69 hn)
        (note 67 qn)
        (note 67 qn)
        (note 67 hn)
        (note 69 qn)
        (note 69 qn)
        (note 69 hn)
        (note 69 qn)
        (note 67 qn)
        (note 65 qn)
        (note 67 qn)
        (note 69 qn)
        (note 69 qn)
        (note 69 hn)
        (note 67 qn)
        (note 67 qn)
        (note 69 qn)
        (note 67 qn)
        (note 65 hn)
        (note 65 hn)
    )
)

naive-mary-had-a-little-lamb

;;; i)
;;; (power-chord nt dr)
;;; nt: composition?
;;; dr: dur?
;;; If note is entered, the function will play the par of two notes: note itself and (note nt+7 dr).
;;; If other inputs that are not related to notes (non-int or non-duration) are entered, it will cause an error.

(define power-chord (lambda (nt dr)
        (par
            (note nt dr)
            (note (+ nt 7) dr)
        )
    )
)
(power-chord 69 wn)

;;; ii)
;;; nt: composition?
;;; dr: dur?
;;; If note is entered, the function will play the par of four notes: note itself, (note nt+3 dr), (note nt+7 dr), and (note nt+10 dr).
;;; If other inputs that are not related to notes (non-int or non-duration) are entered, it will cause an error.

(define minor-7th (lambda (nt dr)
        (par
            (note nt dr)
            (note (+ nt 3) dr)
            (note (+ nt 7) dr)
            (note (+ nt 10) dr)
        )
    )
)
(minor-7th 69 wn)

;;; iii)
;;; nt: composition?
;;; dr: dur?
;;; If note is entered, the function will determine its index from the ASCII table, and process it to return the equivalent pitch integer value.
;;; If other inputs that are not related to notes (non-int or non-duration) are entered, it will return an error.
;;; Since they are arranged as the keyboards, they must be programmed carefully. For example, nt = E# does not exist.

(define pitch (lambda (nt)
        (cond
            [(and (< 1 (string-length nt)) (>= 2 (string-length nt)))
                (cond
                    [(or (string=? nt "E#") (string=? nt "Fb") (string=? nt "Cb") (string=? nt "B#"))
                        (error "Invalid pitch given: [pitch]")
                    ]
                    [(or (string=? (substring nt 0 1) "A") (string=? (substring nt 0 1) "B"))
                        (cond
                            [(string=? (substring nt 1 2) "#") 
                                (- (* (char->integer (string-ref nt 0)) 2) 60)
                            ]
                            [(string=? (substring nt 1 2) "b") 
                                (- (* (char->integer (string-ref nt 0)) 2) 62)
                            ]
                        )
                    ]
                    [(< (char->integer (string-ref nt 0)) 70) 
                        (cond
                            [(string=? (substring nt 1 2) "#") 
                                (- (* (char->integer (string-ref nt 0)) 2) 73)
                            ]
                            [(string=? (substring nt 1 2) "b") 
                                (- (* (char->integer (string-ref nt 0)) 2) 75)
                            ]
                        )
                    ]
                    [else
                        (cond
                            [(string=? (substring nt 1 2) "#") 
                                (- (* (char->integer (string-ref nt 0)) 2) 74)
                            ]
                            [(string=? (substring nt 1 2) "b") 
                                (- (* (char->integer (string-ref nt 0)) 2) 76)
                            ]
                        )    
                    ]
                )
            ]
            [(= 1 (string-length nt)) 
                (cond
                    [(or (string=? (substring nt 0 1) "A") (string=? (substring nt 0 1) "B"))
                        (- (* (char->integer (string-ref nt 0)) 2) 61)
                    ]
                    [(< (char->integer (string-ref nt 0)) 70) 
                        (- (* (char->integer (string-ref nt 0)) 2) 74)
                    ]
                    [else
                        (- (* (char->integer (string-ref nt 0)) 2) 75)
                    ]
                )
            ]
            [else 
                (error "Invalid pitch given: [pitch]")
            ]
        )
    )
)

(pitch "B")

(define mary-had-a-little-lamb
    (let*
        (
            [chor1_sub1
                (seq 
                    (note (- (pitch "C") 12) qn) (note (- (pitch "G") 12) qn) 
                    (note (- (pitch "E") 12) qn) (note (- (pitch "G") 12) qn)
                )
            ]
            [chor1_sub2
                (seq (note (- (pitch "B") 24) qn) (note (- (pitch "G") 12) qn) 
                            (note (- (pitch "D") 12) qn) (note (- (pitch "G") 12) qn))
            ]
            [chor1
                (seq
                    (par 
                        (seq (power-chord (pitch "A") hn) 
                            (power-chord (pitch "G") hn)) 
                        chor1_sub1)
                    (par 
                        (seq (power-chord (pitch "F") hn) 
                            (power-chord (pitch "G") hn)) 
                        chor1_sub1)
                    (par 
                        (seq (power-chord (pitch "A") hn) 
                            (power-chord (pitch "A") hn)) 
                        chor1_sub1)
                    (par 
                        (seq (power-chord (pitch "A") wn)) 
                        chor1_sub1)
                    (par 
                        (seq (power-chord (pitch "G") hn) 
                            (power-chord (pitch "G") hn)) 
                        chor1_sub2)
                )
            ]
            [chor_climax_sub1
                (seq (note (- (pitch "C") 12) qn) (note (- (pitch "A") 12) qn) 
                    (note (- (pitch "F") 12) qn) (note (- (pitch "A") 12) qn))
            ]
            [chor_climax
                (seq
                    (par 
                        (seq (power-chord (pitch "G") wn)) 
                        chor1_sub1
                    )
                    (par 
                        (seq (power-chord (pitch "A") hn) 
                            (power-chord (+ (pitch "C") 12) hn)) 
                        chor_climax_sub1)
                    (par 
                        (seq (power-chord (+ (pitch "C") 12) wn)) 
                        chor_climax_sub1)
                )
            ]
            [chor_epilogue
                (seq
                    (par 
                        (seq (power-chord (pitch "A") hn) 
                            (power-chord (pitch "G") hn)) 
                        (seq (note (- (pitch "C") 12) qn) (note (- (pitch "G") 12) qn) 
                            (note (- (pitch "E") 12) qn) (note (- (pitch "G") 12) qn)))
                    (par 
                        (seq (power-chord (pitch "F") wn) 
                            (power-chord (pitch "F") wn)) 
                        (seq (par (note (- (pitch "C") 12) wn) (note (- (pitch "G") 12) wn) (note (- (pitch "E") 12) wn))
                            (par (note (- (pitch "C") 12) wn) (note (- (pitch "G") 12) wn) (note (- (pitch "E") 12) wn))))
                )
            ]
        )
        (seq 
            chor1
            chor_climax
            chor1
            chor_epilogue
        )
    )
)

mary-had-a-little-lamb