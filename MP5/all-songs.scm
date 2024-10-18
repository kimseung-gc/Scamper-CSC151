(import music)
(import image)

;; CSC-151-03 (Fall 2022)
;; Mini-Project 5: Musical copyright
;; Seunghyeon Kim
;; 2022-10-06
;; ACKNOWLEDGEMENTS:
;; ...

"1. all-pairs"
; If lst is null, all-pairs return null
; If lst is not null, all-pairs return the pair of v and the head 
; and the cons of the all-pairs of its tail.
; (all-pairs v lst): list? (of pairs)
; v: any?
; lst: list?
; It returns the pair of v and each element of lst
(define all-pairs
    (lambda (v lst)
        (match lst
            [null null]
            [(cons head tail) (cons (pair v head) (all-pairs v tail))]
        )
    )
)
(test-case "all-pairs" equal? (list (list null 43 4 3 2) (list null "a" #\f) (pair null 3)) (all-pairs null (list (list 43 4 3 2) (list "a" #\f) 3)))
(test-case "all-pairs" equal? (list (list (list 1 2 3) (range 10) 4 3 2) (list (list 1 2 3) "a" #\f) (pair (list 1 2 3) 3)) (all-pairs (list 1 2 3) (list (list (range 10) 4 3 2) (list "a" #\f) 3)))
(test-case "all-pairs" equal? (list (list #\a (range 10) 4 null 2) (list #\a "a" #\f) (pair #\a 3)) (all-pairs #\a (list (list (range 10) 4 null 2) (list "a" #\f) 3)))

"2. cartesian-product"
; If lst is null, cartesian-product return null
; If lst is not null, cartesian-product return all-pairs of each element of l1 
; and each element of l2 and the cons of the cartesian-product of l1's tail and l2.
; (cartesian-product l1 l2): list?
; l1: list?
; l2: list?
; it returns the cartesian pairs of the lists l1 and l2.
(define cartesian-product
    (lambda (l1 l2)
        (match l1
            [null null]
            [(cons h1 t1) (append (all-pairs h1 l2) (cartesian-product t1 l2))]
        )
    )
)
(cartesian-product (range 3) (list "a" "b"))
(test-case "cartesian-product" equal? (list (list null 43 4 3 2) (list null "a" #\f) (pair null 3)) (all-pairs null (list (list 43 4 3 2) (list "a" #\f) 3)))
(test-case "cartesian-product" equal? (list (list (list 1 2 3) (range 10) 4 3 2) (list (list 1 2 3) "a" #\f) (pair (list 1 2 3) 3)) (all-pairs (list 1 2 3) (list (list (range 10) 4 3 2) (list "a" #\f) 3)))
(test-case "cartesian-product" equal? (list (list #\a (range 10) 4 null 2) (list #\a "a" #\f) (pair #\a 3)) (all-pairs #\a (list (list (range 10) 4 null 2) (list "a" #\f) 3)))

"2.5 Helper function of all-two-note-songs"
; If NTC is null, matchNtPair returns null.
; If NTC is not null, matchNtPair returns (cons (seq (note (car head) qn) (note (cdr head) qn)) (matchNtPair tail)).
; (matchNtPair NTC): list?
; NTC: list? (the list of combination of all the MIDI notes)
; it returns the sequencial notes of the cartesian pairs of the (cartesian-product NTC NTC)
(define matchNtPair
    (lambda (NTC) 
        (match NTC 
            [null null] 
            [(cons head tail) 
                (cons (seq (note (car head) qn) (note (cdr head) qn)) (matchNtPair tail))
            ]
        )
    )
)

"3. all-two-note-songs"
; If notes is null, all-two-note-songs return null
; If notes is not null, it returns the note of the cartesian-product of the notes and notes.
; (all-two-note-songs notes): list?
; all-two-note-songs: integer list? (consist of MIDI notes)
; it returns the list of sequencial notes of the cartesian pairs of the (cartesian-product NTC NTC)
(define all-two-note-songs
    (lambda (notes)
        (let*
            (
                [noteComb (cartesian-product notes notes)]
            )
            (matchNtPair noteComb)
        )
        
    )
)
(define two-note-example
    (all-two-note-songs (list 69 60))
)
two-note-example
;(test-case "two-note songs, 60 69" equal? (list (seq (note 60 qn) (note 60 qn)) (seq (note 60 qn) (note 69 qn)) (seq (note 69 qn) (note 60 qn)) (seq (note 69 qn) (note 69 qn))) (all-two-note-songs (list 60 69)))
; test-case does not work for the music library...

"4. cons-all"
; If lsts is null, cons-all returns null
; If lsts is not null, cons-all returns the pair of each of the elements of lsts with
; x in the front.
; (cons-all x lsts): list?
; x: any?
; lsts: 2D list?
; It returns a list of x appended to each list of lsts.
(define cons-all
    (lambda (x lsts)
        (match lsts
            [null null]
            [(cons head tail) (cons (cons x head) (cons-all x tail))]
        )
    )
)
(cons-all 0 (list (list 1 2) (list 3 4 5) (list 6 7)))
(test-case "cons for 3 lsts" equal? (list (list 0 1 2) (list 0 3 4 5) (list 0 6 7)) (cons-all 0 (list (list 1 2) (list 3 4 5) (list 6 7))))
(test-case "cons for null and 3 lsts" equal? (list (list null 1 2) (list null 3 4 5) (list null 6 7)) (cons-all null (list (list 1 2) (list 3 4 5) (list 6 7))))
(test-case "cons for 2 nulls" equal? (list (list 0) (list 0)) (cons-all 0 (list null null)))

"5. combinations"
; If lsts null, it returns the (list null)
; If lsts contain more than one element, it returns the cons-all of the head and the 
; combinations of the tail, and it straightens the 2D list into 1D list by applying 
; append to each of the recurred elements.
; (combinations lsts): list?
; lsts: list?
; It returns the list of combinations of the elements of each input list.
"solution A"
(define combinations_A
    (lambda (lsts)
        (match lsts
            [null (list null)]
            [(cons head tail) 
                (if (equal? tail (list null))
                    (list null)
                    (apply append (map (lambda (x) (cons-all x (combinations_A tail))) head))
                )
            ]
        )
    )
)
(test-case "combinations of multiple null lists" equal? (list null) (combinations_A (list null null)))
(test-case "combinations of 3 different lists" equal? (list (list 1 3 6) (list 1 3 7) (list 1 4 6) (list 1 4 7) (list 1 5 6) (list 1 5 7) (list 2 3 6) (list 2 3 7) (list 2 4 6) (list 2 4 7) (list 2 5 6) (list 2 5 7)) (combinations_A (list (list 1 2) (list 3 4 5) (list 6 7))))
(test-case "combinations of null" equal? (list null) (combinations_A null))
(test-case "combinations of 2 different lists" equal? (list (list 0 "a") (list 0 "b") (list 1 "a") (list 1 "b") (list 2 "a") (list 2 "b")) (combinations_A (list (range 3)(list "a" "b"))))
"solution B"
(define combinations_B
    (lambda (lsts)
        (match lsts
            [null (list null)]
            [(cons head tail) 
                (if (equal? tail (list null))
                    (list null)
                    (cartesian-product head (combinations_B tail)))
                ]; Utilizes the cartesian product of two lsts
        )
    )
)
(length (combinations_B (list (range 3) (range 3) (list "a" "b"))))
(test-case "combinations of multiple null lists" equal? (list null) (combinations_B (list null null)))
(test-case "combinations of 3 different lists" equal? (list (list 1 3 6) (list 1 3 7) (list 1 4 6) (list 1 4 7) (list 1 5 6) (list 1 5 7) (list 2 3 6) (list 2 3 7) (list 2 4 6) (list 2 4 7) (list 2 5 6) (list 2 5 7)) (combinations_B (list (list 1 2) (list 3 4 5) (list 6 7))))
(test-case "combinations of null" equal? (list null) (combinations_B null))
(test-case "combinations of 2 different lists" equal? (list (list 0 "a") (list 0 "b") (list 1 "a") (list 1 "b") (list 2 "a") (list 2 "b")) (combinations_B (list (range 3)(list "a" "b"))))

"5.5 all-songs helper"
; If lst = null, it returns an empty note
; If lst is not null, it returns the sequence of each note of lst
; (combinatory_helper lst): note composition list?
; lst: integer list?
; It returns list of each integers of lst input into (note integer qn).
; It is used as a helper function for all-songs.
(define combinatory_helper
    (lambda (lst)
        (match lst
            [null empty]
            [(cons head tail) 
                (if (integer? head)
                    (seq (note head qn) (combinatory_helper tail))
                    (seq head (combinatory_helper tail))
                )
            ]
        )
    )
)
; If n=0, it returns null
; If n≠0, it returns the lst and recurrs the function by subtracting 
; n by 1.
; (makeL n lst): list?
; n: integer?
; lst: list?
; It returns the list of n copies of the lst which does the same
; function as make-list.
(define makeL
    (lambda (n lst)
        (if (equal? n 0)
            null
            (cons lst (makeL (- n 1) lst))
        )
    )
)

"6. all-songs function"
; (all-songs n notes): note list?
; n: integer? (non-negative)
; notes: integer? (MIDI notes of the notes)
; It returns the note compositions with the combinations of n lists
; that sequential connection.
(define all-songs 
    (lambda (n lst)
        (map combinatory_helper (combinations_B (makeL n lst)))
    )
)

(define five-note-example
    (list 60 63 65)
)

(all-songs 5 five-note-example); It has a long running time, so becareful...

; I think that music is a type of subject that requires a lot of emotional
; aspects. For now, computers cannot represent human emotions yet, so modern 
; machines cannot judge the work that involves emotions. Thus, the modern 
; computers cannot yet be called as "do it all."

; First of all, computers cannot calculate humans' emotions, meaning they
; may not be able to accurately interpret whether a song is sad or happy.
; In that case, we can deduce that they may consider C major song and a 
; C minor song as copyright targets since they only have a minor difference.
; In human perspective, however, they have completely different moods which
; computers cannot compute. As a result, I think that it is too early to 
; state that we can totally rely on computers for musical analysis. According 
; to what I have heard from professors about the 8 note copyrights, I think
; they should have asked the musical professionals about it instead of 
; computers that cannot understand human emotions.

; Also, I think that if computers are implemented to the extent that it can
; create its own song, and based on what I did in the mini-project today,
; I do not understand why people are freaking out on whether computers converge
; as a "do it all" machine since we cannot create complex songs through only
; combinations since there exists approximately googol number songs with 12
; notes (the number of semitones in an octave) and length of 90. I see VS
; code collapsing due to stack over flow. Thus, I argue that it is too early 
; to say that computers are at "do it all" state.

; When it becomes the "do it all" state, however, I think that computers will
; be able to accurately calculate our emotions, and create decent songs. 
; Nevertheless, I would like to argue that reinforcement has its limits, so
; unless people find out a new way to interpret music that will interest
; other people, only computers in the musics field will make the beautiful
; music field that we currently have lost in our memories.