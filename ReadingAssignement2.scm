"1."
(define shorter?
    (lambda (str1 str2) 
        (< (string-length str1) (string-length str2)))
)

(shorter? "" "a")

"2."

"a."
(define odd-even
    (lambda (num) 
        (if (= (remainder num 2) 1) 
            (* 2 num)
            (/ num 2)
        )
    )
)

(odd-even 3)

"b."

(define pos-neg
    (lambda (num) 
        (cond 
            [(> num 0) "+"] 
            [(< num 0) "-"] 
            [(zero? num) "neither"]
        )
    )
)
(pos-neg 0)