(import music)
(define ghost 
    (lambda (md-nt dr) 
        (mod 
            percussion
            (mod (dynamics 32); modifies the input note quieter than normal is the number is less than 64
            (note md-nt dr))
        )
    )
)
(define accent 
    (lambda (md-nt dr) 
        (mod 
            percussion
            (mod (dynamics 127); modifies the input note louder than normal is the number is bigger than 64
            (note md-nt dr))
        )
    )
)
(define beat_machine
    (lambda (n str)
        (let*
            (
                [bt1_ntr1
                    35
                ]
                [bt1_row1
                    (mod
                        percussion
                        (seq
                            (accent bt1_ntr1 en)
                            (accent bt1_ntr1 en)
                            (rest en)
                            (accent bt1_ntr1 sn)
                            (accent bt1_ntr1 sn)
                            (rest sn)
                            (accent bt1_ntr1 sn)
                            (accent bt1_ntr1 en)
                            (rest en)
                        )
                    )
                ]
                [bt1_ntr2
                    38
                ]
                [bt1_row2
                    (mod
                        percussion
                        (seq
                            (rest qn)
                            (note bt1_ntr2 en)
                            (rest en)
                            (rest qn)
                            (note bt1_ntr2 qn)
                        )
                    )
                ]
                [bt1; beat 1 is from Changmo's song called Meteor (https://youtu.be/lOrU0MH0bMk)
                    (par
                        bt1_row1
                        bt1_row2
                    )
                ]
                ;;; bt1 ends here.
                [bt2_ntr1
                    35
                ]
                [bt2_row1
                    (mod
                        percussion
                        (seq
                            (note bt2_ntr1 qn)
                            (rest qn)
                            (note bt2_ntr1 en)
                            (rest en)
                            (note bt2_ntr1 qn)
                        )
                    )
                ]
                [bt2_ntr2
                    38
                ]
                [bt2_row2
                    (mod
                        percussion
                        (seq
                            (rest qn)
                            (note bt2_ntr2 qn)
                            (rest en)
                            (note bt2_ntr2 en)
                            (rest qn)
                        )
                    )
                ]
                [bt2
                    (par
                        bt2_row1
                        bt2_row2
                    )
                ]
                ;;; bt2 ends here.
                [bt3_ntr1
                    46
                ]
                [bt3_row1
                    (mod
                        percussion
                        (seq
                            (rest en)
                            (note bt3_ntr1 qn)
                            (rest en)
                            (note bt3_ntr1 en)
                            (rest qn)
                            (rest en)
                        )
                    )
                ]
                [bt3_ntr2
                    35
                ]
                [bt3_row2
                    (mod
                        percussion
                        (seq
                            (note bt3_ntr2 en)
                            (rest qn)
                            (note bt3_ntr2 en)
                            (rest en)
                            (note bt3_ntr2 qn)
                            (note bt3_ntr2 en)
                        )
                    )
                ]
                [bt3
                    (par
                        bt3_row1
                        bt3_row2
                    )
                ]
            )
            (cond
                [(string=? str "123") (repeat n (par bt1 bt2 bt3))]
                [(string=? str "12") (repeat n (par bt1 bt2))]
                [(string=? str "23") (repeat n (par bt3 bt2))]
                [(string=? str "13") (repeat n (par bt1 bt3))]
                [(string=? str "1") (repeat n bt1)]
                [(string=? str "2") (repeat n bt2)]
                [(string=? str "3") (repeat n bt3)]
            )
        )
    )
)
(seq
    (beat_machine 2 "1")
    (beat_machine 1 "12")
    (beat_machine 1 "23")
    (beat_machine 3 "123")
)
(define beat_machine
    (lambda (n l1 l2 l3)
        (let
            (
                [BAM
                    (lambda 
                        (n nt ga? dr) 
                        (repeat n 
                            (cond 
                                [(equal? ga? -1) (rest dr)]
                                [(equal? ga? 1) (ghost nt dr)]
                                [(equal? ga? 2) (accent nt dr)]
                                [else (mod percussion (note nt dr))]
                            )
                        )
                    )
                ]
                [n1
                    (car l1)
                ]
                [n2
                    (car l2)
                ]
                [n3
                    (car l3)
                ]
                [ga1
                    (cdr l1)
                ]
                [ga2
                    (cdr l2)
                ]
                [ga3
                    (cdr l3)
                ]
                [dr1
                    (car (reverse l1))
                ]
                [dr2
                    (car (reverse l2))
                ]
                [dr3
                    (car (reverse l3))
                ]
            )
            (repeat n
                (par
                    (seq (BAM n1 35 ga1 dr1))
                    (seq (BAM n2 38 ga2 dr2))
                    (seq (BAM n3 46 ga3 dr3))
                )
            )
        )
    )
)
(let
    (
        [l1
            (list 2 2 en)
        ]
        [l2
            (list 2 -1 en)
        ]
        [l3
            (list 2 2 en)
        ]
    )
    (beat_machine 1 l1 l2 l3)
)
