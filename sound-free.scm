(import music)

(define pitch (lambda (nt)
        (cond
            [(and (< 1 (string-length nt)) (>= 2 (string-length nt)))
                (cond
                    [(or (string=? nt "E#") (string=? nt "Fb") (string=? nt "Cb") (string=? nt "B#"))
                        (error "Invalid pitch given: [pitch]")
                    ]
                    [(or (string=? (substring nt 0 1) "A") (string=? (substring nt 0 1) "B"))
                        (- (* (char->integer (string-ref nt 0)) 2) 60)]
                    [(< (char->integer (string-ref nt 0)) 70) 
                        (- (* (char->integer (string-ref nt 0)) 2) 73)]
                    [else
                        (- (* (char->integer (string-ref nt 0)) 2) 74)]
                )
            ]
            [(= 1 (string-length nt)) 
                (cond
                    [(or (string=? (substring nt 0 1) "A") (string=? (substring nt 0 1) "B"))
                        (- (* (char->integer (string-ref nt 0)) 2) 61)]
                    [(< (char->integer (string-ref nt 0)) 70) 
                        (- (* (char->integer (string-ref nt 0)) 2) 74)]
                    [else
                        (- (* (char->integer (string-ref nt 0)) 2) 75)]
                )
            ]
            [else 
                (error "Invalid pitch given: [pitch]")
            ]
        )
    )
)

(define octave (lambda (nt oct)
        (note (+ (pitch nt) (* 12 (- oct 4))) qn)
    )
)

(define NT 
    (lambda (ntC)
        (note 
            (octave 
                (substring ntC 0 2) 
                (string->number (substring ntC 2 3))
            ) 
        qn)
    )
)

(define Moon3rd
    (seq 
        (NT "C#1")
        (NT "G#1")
        (par (NT "G#1") (NT ""))
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
        (note 69 qn)
        (note 67 qn)
        (note 67 qn)
        (note 69 qn)
        (note 67 qn)
        (note 65 hn)
        (note 65 hn)
    )
)