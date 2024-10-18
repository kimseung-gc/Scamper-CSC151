(import music)
(import image)

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

"2.5 Helper function of all-two-note-songs"
; If NTC is null, matchNtPair returns null.
; If NTC is not null, matchNtPair returns (cons (seq (note (car head) qn) (note (cdr head) qn)) (matchNtPair tail)).
; (matchNtPair NTC): list?
; NTC: list? (the list of combination of all the MIDI notes)
; it returns the sequencial notes of the cartesian pairs of the (cartesian-product NTC NTC)
(define matchNtPair
    (lambda (NTP) 
        (match NTP
            [null empty]
            [(cons head tail) (seq (note (car NTP) qn) (matchNtPair (cdr NTP)))]
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
        (map 
            (lambda (NTP) 
                (match NTP
                    [null empty]
                    [(cons head tail) (seq (note (car NTP) qn) (matchNtPair (cdr NTP)))]))
            (cartesian-product notes notes))))