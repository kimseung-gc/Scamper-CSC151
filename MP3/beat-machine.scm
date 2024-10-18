;; CSC-151-03 (Fall 2022)
;; Mini-Project 3: Beat Machine
;; Seunghyeon Kim
;; 2022-09-23
;; ACKNOWLEDGEMENTS:
;;   Sarah Thawyer, via Drumeo’s Exploring Indian Grooves course
(import music)
;;; part 1
;;; (accent md-nt dur): composition?
;;; md-nt: composition?
;;; dur: dur?
;;; It returns a note with louder sound.
;;; If (accent 69 qn),
;;;     returns (mod (dynamics int bigger than 64) (note C# qn))
;;; If input not(composition)
;;;     error
;(note 38 qn)
;(mod (dynamics 127) (note 38 qn))
(define accent 
    (lambda (md-nt dr) 
        (mod 
            percussion
            (mod (dynamics 127); modifies the input note louder than normal is the number is bigger than 64
            (note md-nt dr))
        )
    )
)
(accent 38 hn)
;;; (ghost md-nt dur): composition?
;;; md-nt: composition?
;;; dur: dur?
;;; It returns a note with quieter sound.
;;; If (accent 39 qn),
;;;     returns (mod (dynamics int smaller than 64) (note 39 qn))
;;; If input not(composition)
;;;     error
(define ghost 
    (lambda (md-nt dr) 
        (mod 
            percussion
            (mod (dynamics 32); modifies the input note quieter than normal is the number is less than 64
            (note md-nt dr))
        )
    )
)
(ghost 42 hn)
(define strokes
    (mod
        percussion
        (seq 
            (accent 42 qn) 
            (ghost 42 qn) 
            (ghost 42 qn) 
            (note 42 qn) 
            (accent 42 qn) 
            (ghost 42 qn) 
            (ghost 42 qn) 
            (note 42 qn) 
        )
    )
)
strokes
;;; (tremolo slashes md-nt dr): composition?
;;; slashes: integer?(>= 0)
;;; md-nt: composition?
;;; dur: dur?
;;; It returns a composition of a note with a 
;;; duration of (denominator dr)*(expt 2 slashes) 
;;; repeating for 2^(slashes)
;;; If (tremolo 1 69 qn),
;;;     returns repeating notes of C# with duration en
;;; If input not(composition)
;;;     error
(define tremolo 
    (lambda (slashes md-nt dr)
        (mod
            percussion
            (repeat (expt 2 slashes); repeating the code for 2^slashes
                (note md-nt 
                    (dur (numerator dr) (* (denominator dr) (expt 2 slashes)))
                )
            )
        )
    )
)
(repeat 10 (tremolo 3 38 hn))
;;; (roll md-nt dr): composition?
;;; md-nt: composition?
;;; dur: dur?
;;; It returns a composition of a note with a 
;;; duration of (denominator dr)/4 
;;; repeating 4 times
;;; If (roll 69 dr),
;;;     returns repeating notes of C# with duration dr/4
;;; If input not(composition)
;;;     error
(define roll 
    (lambda (md-nt dr)
        (mod
            percussion
            (
                repeat
                4; repeating the code for 4 times
                (note md-nt 
                    (dur (numerator dr) (* (denominator dr) 4))
                )
            )
        )
    )
)
(roll 68 hn)
;;; (flam md-nt dr): composition?
;;; md-nt: composition?
;;; dr: dur?
;;; It returns a composition of a note with a 
;;; duration of (denominator dr)/2
;;; and an accented normal note with duration dr.
;;; If (roll 69 dr),
;;;     returns notes of C# with duration dr/2 and accented C#
;;; If input not(composition)
;;;     error
(define flam 
    (lambda (md-nt dr)
        (mod 
            percussion
            (seq 
                (note md-nt (dur (numerator dr) (* (denominator dr) 2)))
                (accent md-nt dr)
            )
        )
    )
)
(flam 68 qn)
;;; (single-drag-tap md-nt): composition?
;;; md-nt: composition?
;;; It returns a composition of a repeating 
;;; note with a duration of en/2 twice.
;;; and then it plays the note without accent for
;;; duration en, and then the note with accent for
;;; duration en.
;;; If (roll 69 dr),
;;;     returns C# with a duration of en/2 twice
;;; C# without accent for
;;; duration en, and then C# with accent for
;;; duration en.
;;; If input not(composition)
;;;     error
(define single-drag-tap
    (lambda (md-nt)
        (mod
            percussion
            (seq
                (tremolo 1 md-nt en)
                (note md-nt en)
                (accent md-nt en)
            )
        )
    )
)
(single-drag-tap 68)
;;; Part 2
(define hor1
    (tremolo 2 42 wn)
)
(define hor2
    (mod percussion 
        (seq
            (rest qn)
            (note 38 qn)
            (rest qn)
            (note 38 qn)
        )
    )
)
(define hor3
    (mod percussion 
        (seq
            (note 35 qn)
            (rest qn)
            (note 35 qn)
            (rest qn)
        )
    )
)
(define horizontal-simple-rock-beat
    (par
        hor1
        hor2
        hor3
    )
)
horizontal-simple-rock-beat
(define ver1
    (mod percussion
        (par
            (note 42 qn)
            (note 35 qn)
        )
    )
)
(define ver2
    (mod percussion
        (par
            (note 42 qn)
            (note 38 qn)
        )
    )
)
(define vertical-simple-rock-beat
    (seq
        ver1
        ver2
        ver1
        ver2
    )
)
vertical-simple-rock-beat
;;; (beat_machine_hor insNt PseqL): composition?
;;; It takes in 2 inputs as 
;;; insNt: integer?(42, 38, or 35)
;;; PseqL: list? (it consists of another list in the format (list n_x, ga?_x, dr_x))
;;; n_x: integer?
;;; ga?_x: string?("s", "n", "g", or "a")
;;; dr_x: dur?
;;; It returns a horizontal composition (row) of repeated note of nt for 
;;; duration dr. The variable sga? indicates whether the note should be in 
;;; ghost, silent, or accent. When "n," it is neutral meaning the function will return
;;; note with percussion only without change in dynamics. I used horizontal
;;; decomposition since vertical decomposition has a lot of limitations such as
;;; change in voice notes with an extended other voice.
;;; If (beat_machine 1 (list (list 1 "n" qn) (list 1 "n" qn) (list 1 "n" qn))),
;;;     returns all three beats with a duration of qn once without any change in dynamics.
(define beat_machine_hor
    (lambda (insNt PseqL)
        (let*
            (
                [BAM
                    (lambda (n nt sga? dr) 
                        (repeat n 
                            (cond 
                                [(string=? sga? "s") (rest dr)]
                                [(string=? sga? "g") (ghost nt dr)]
                                [(string=? sga? "a") (accent nt dr)]
                                [else (mod percussion (note nt dr))]
                            )
                        )
                    )
                ]
                [Ps1
                    (car PseqL)
                ]
                [n1
                    (car Ps1)
                ]
                [sga1
                    (car (cdr Ps1))
                ]
                [dr1
                    (car (reverse Ps1))
                ]
            )
            (if (equal? 1 (length PseqL))
                (BAM n1 insNt sga1 dr1)
                (seq
                    (BAM n1 insNt sga1 dr1)
                    (beat_machine_hor insNt (cdr PseqL))
                )
            )
        )
    )
)
;;; (beat_machine n insL seqL): composition?
;;; n: integer?
;;; seqL: list? (length of 3)
;;; insL: list? (length of 3)
;;; It uses the helper function beat_machine_hor, and it 
;;; returns a whole sequence coded by seqL that is repeated 
;;; n times with the instruments in insL. For most of the
;;; beats, the insL will remain (list 42 38 35). 
(define beat_machine
    (lambda (n insL seqL)
        (let
            (
                [hor1
                    (car seqL)
                ]
                [hor2
                    (car (cdr seqL))
                ]
                [hor3
                    (car (reverse seqL))
                ]
                [ins1
                    (car insL)
                ]
                [ins2
                    (car (cdr insL))
                ]
                [ins3
                    (car (reverse insL))
                ]
            )
            (repeat n
                (if (zero? (length insL))
                    (empty)
                    (par
                        (beat_machine_hor ins1 hor1)
                        (beat_machine_hor ins2 hor2)
                        (beat_machine_hor ins3 hor3)
                    )
                )
            )
        )
    )
)
(define simple-rock-beat
    (beat_machine
        1
        (list 42 38 35)
        (list
            (list (list 4 "n" qn))
            (list (list 1 "s" qn) (list 1 "n" qn) (list 1 "s" qn) (list 1 "n" qn))
            (list (list 1 "n" qn) (list 1 "s" qn) (list 1 "n" qn) (list 1 "s" qn))
        )
    )
)
"simple-rock-beat"
simple-rock-beat
(define elaborate-rock-beat
    (beat_machine
        1
        (list 42 38 35)
        (list
            (list (list 8 "n" en))
            (list (list 2 "g" en) (list 1 "a" en) (list 2 "g" en) (list 1 "a" en) (list 2 "g" en))
            (list (list 2 "n" en) (list 1 "s" en) (list 1 "n" en) (list 3 "s" en) (list 1 "n" en))
        )
    )
)
"elaborate-rock-beat"
elaborate-rock-beat
(define latin-beat
    (beat_machine
        1
        (list 42 38 35)
        (list
            (list (list 8 "n" en))
            (list (list 2 "s" en) (list 1 "n" en) (list 2 "s" en) (list 1 "n" en) (list 2 "s" en))
            (list (list 1 "n" qn) (list 1 "s" en) (list 1 "n" en) (list 1 "n" qn) (list 1 "s" en) (list 1 "n" en))
        )
    )
)
"latin-beat"
latin-beat
(define swing-beat
    (beat_machine
        1
        (list 42 38 35)
        (list
            (list (list 1 "n" qn) (list 2 "n" en) (list 1 "n" qn) (list 2 "n" en))
            (list (list 1 "s" qn) (list 1 "n" en) (list 1 "s" (dur 3 8)) (list 1 "n" en) (list 1 "s" en))
            (list (list 4 "n" qn))
        )
    )
)
"swing-beat"
swing-beat
(define funk-beat
    (beat_machine
        2
        (list 42 38 35)
        (list
            (list (list 2 "n" en) (list 1 "a" en) (list 1 "n" sn) (list 1 "s" sn) (list 1 "n" sn) (list 1 "s" sn) (list 1 "n" sn) (list 1 "s" sn) (list 1 "a" en) (list 1 "n" sn) (list 1 "s" sn))
            (list (list 1 "s" qn) (list 1 "a" en) (list 1 "s" sn) (list 1 "g" sn) (list 1 "s" sn) (list 2 "g" sn) (list 1 "s" sn) (list 1 "a" en) (list 1 "s" sn) (list 1 "g" sn))
            (list (list 2 "n" en) (list 1 "s" hn) (list 1 "s" sn) (list 1 "n" sn) (list 1 "s" en) (list 1 "n" en))
        )
    )
)
"funk-beat"
funk-beat
(define garba-beat
    (beat_machine
        1
        (list 48 38 35)
        (list
            (list (list 1 "s" (dur 3 4)) (list 3 "n" sn) (list 1 "s" sn))
            (list (list 1 "s" en) (list 1 "n" en) (list 1 "s" en) (list 1 "n" en) (list 3 "s" tn) (list 1 "n" tn) (list 1 "s" sn) (list 1 "n" sn) (list 3 "s" sn) (list 1 "n" en))
            (list (list 1 "n" en) (list 1 "s" en) (list 1 "n" en) (list 1 "s" en) (list 1 "n" tn))
        )
    )
)
"garba-beat"
garba-beat
(define own-beat
    (beat_machine
        3
        (list 44 38 35)
        (list
            (list (list 1 "s" qn) (list 1 "n" sn) (list 1 "g" en) (list 1 "n" sn) (list 1 "g" sn) (list 1 "n" sn) (list 1 "g" en) (list 1 "n" en) (list 1 "s" en))
            (list (list 1 "s" qn) (list 1 "n" sn) (list 1 "s" en) (list 1 "n" sn) (list 1 "s" sn) (list 1 "n" sn) (list 1 "s" en) (list 1 "n" en) (list 1 "s" en))
            (list (list 2 "n" en) (list 1 "s" (dur 5 8)) (list 1 "n" en))
        )
    )
)
"own-beat"
own-beat
(mod (tempo qn 1000) own-beat); too fast
(mod (tempo qn 500) own-beat); machine gun
(mod (tempo qn 200) own-beat); fast... not good for my beat
(mod (tempo qn 120) own-beat); similar to human heart-beat
(mod (tempo qn 140) own-beat)