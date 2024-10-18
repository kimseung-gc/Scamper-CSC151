;; CSC 151 (Fall 2022))
;; Lab: Implementing the Big Three
;; Authors: Joyce Gill, Seunghyeon Kim
;; Date: 12/10/2022
;; Acknowledgements:
;;   ACKNOWLEDGEMENTS HERE

;; Whew! We've spent a solid week and a half drilling recursive design 
;; techniques over lists and the natural numbers. But this only the beginning!
;; We will deepen our knowledge of recursive design throughout the remainder
;; course as we explore more intricate and complex problems. As a starting
;; point in this conversation, we'll look at the patterns of recursion that
;; we have developed so far and how they relate to the "big three" operations
;; over lists we encountered earlier in the course.

;; -------------------------
"Problem 1: Transformations"
;; -------------------------

;; Consider the following pair of recursive functions:

;;; (double lst) -> list?
;;;   lst: list? of numbers
;;; Returns lst but with every element of lst doubled.
(define double
  (lambda (lst)
    (match lst
      [null null]
      [(cons head tail) (cons (* 2 head) (double tail))])))

(test-case "double empty"
           equal?
           null
           (double null))

(test-case "double non-empty"
           equal?
           (list 0 2 4 6 8)
           (double (range 5)))

;;; (flip lst) -> list?
;;;   lst: list? of booleans
;;; Returns lst but with every element of lst flipped, i.e., #t becomes #f
;;; and #f becomes true
(define flip
  (lambda (lst)
    (match lst
      [null null]
      [(cons head tail) (cons (not head) (flip tail))])))

(test-case "flip empty"
           equal?
           null
           (flip null))

(test-case "flip non-empty"
           equal?
           (list #t #f #f #t #t #f)
           (flip (list #f #t #t #f #f #t)))

;; (Partner A drives!)
;;
;; Follow the style of these two functions to write a similar, third function
;; called digits->nums. (chars->codepoints lst) takes a list of characters as
;; input and returns a list where each character has been turned into its
;; integer codepoint value. For example:
;;
;; (chars->codepoints (list #\a #\0 #\; #\q #\!))
;; > (list 97 48 59 113 33)
;; (chars->codepoints null)
;; > null

;; (chars->codepoints lst): integer list?
;; lst: char list?
;; It returns the list of each character turned as integers. If the
;; input lst is null, it will return null.
(define chars->codepoints
  (lambda (lst)
    (match lst
      [null null]
      [(cons head tail) (cons (char->integer head) (chars->codepoints tail))]
    )
  )
)

(test-case "normal input" equal? (list 97 48 59 113 33) (chars->codepoints (list #\a #\0 #\; #\q #\!)))
(test-case "null input" equal? null (chars->codepoints null))
;; (Partner B drives!)
;;
;; At this point, you should have noticed some redundancy between the
;; three implementations of these functions. In the space below, note which
;; parts of the three functions are shared and what is different between them.
;;
;; Shared: They are all recursive. They all use match. They all return lists. 
;; They return null when lst = null. They apply the function to each of the
;; elements of lst like map.
;;
;; Different: They utilize different functions to the original input so each 
;; function does different things to the input list. 
;;
;; Before moving on, check your work with a member of the course staff!

;; After you have correctly identified the essential difference between the
;; three functions, let's do exactly what we learned at the beginning of this
;; course: write a function that factors out these differences. It turns out
;; that this function is precisely the map function over lists! Follow your
;; nose and implement the map function over lists by factoring out the
;; essential difference in the implementations above and making it a parameter
;; to your function. You should arrive at precisely the same function signature
;; as the Prelude map function. Give test cases for your implementation of map,
;; list-map, that show how you can use list-map to implement the behavior of
;; the three specialized functions above.

;; (list-map f lst): list?
;; f: procedure?
;; lst: list?
;; It returns the list of f of each elements.
(define list-map
  (lambda (f lst)
    (match lst
      [null null]
      [(cons head tail) (cons (f head) (list-map f tail))]
    )
  )
)

(test-case "not map" equal? (flip (list #f #t #f)) (list-map not (list #f #t #f)))
(test-case "char->integer map" equal? (list 97 48 59 113 33) (list-map char->integer (list #\a #\0 #\; #\q #\!)))

;; ------------------
"Problem 2: Deletion"
;; ------------------

;; Let's play the same game of observing similarities between functions and
;; factoring out the differences to create a new function! Consider these
;; specialized functions:

;;; (dropzeroes lst) -> list?
;;;   lst: list? of numbers
;;; Returns lst but with every zero removed from lst.
(define dropzeroes
  (lambda (lst)
    (match lst
      [null null]
      [(cons head tail)
       (if (zero? head) (dropzeroes tail)
                        (cons head (dropzeroes tail)))])))

(test-case "dropzeroes empty"
           equal?
           null
           (dropzeroes null))

(test-case "dropzeroes non-empty"
           equal?
           (list 1 1 2 1)
           (dropzeroes (list 1 0 0 1 2 0 1 0)))

;;; (length-less-than-five lst) -> list?
;;;   lst: list? of strings
;;; Returns lst but with every element with length greater than or equal to
;;; five removed from the output.
(define length-less-than-five
  (lambda (lst)
    (match lst
      [null null]
      [(cons head tail)
       (if (>= (string-length head) 5) (length-less-than-five tail)
                                       (cons head (length-less-than-five tail)))])))

(test-case "length-less-than-five empty"
           equal?
           null
           (length-less-than-five null))

(test-case "length-less-than-five non-empty"
           equal?
           (list "abba" "doo!")
           (length-less-than-five (list "abba" "yabba" "dabba" "doo!")))

;; (Partner B drives!)
;;
;; Follow the style of these two functions to write a similar, third function
;; called dropfalses. (dropfalses lst) takes a list of booleans as input and
;; and returns a lst but with all the #f values removed from the result. For
;; example:
;;
;; (dropfalses (list #t #t #f #f #f #t #f #t))
;; > (list #t #t #t #t)
;; (dropfalses null)
;; > null

;; (dropfalses lst): list?
;; lst: boolean lst?
;; It returns the list of all the falses in the lst dropped. It will return  null when null is input.
(define dropfalses
  (lambda (lst)
    (match lst
      [null null]
      [(cons #t tail) (cons #t (dropfalses tail))]
      [(cons #f tail) (dropfalses tail)]
    )
  )
)

(test-case "standard input" equal? (list #t #t #t #t) (dropfalses (list #t #t #f #f #f #t #f #t)))
(test-case "null input" equal? null (dropfalses null))

;; (Partner A drives!)
;;
;; Like the previous problem, first identify what is shared and different
;; between these three functions:
;;
;; Shared: They may involve if statements inside the match (not in our case since we used the cleaner way to express the code). 
;; They behave like the filter function where it filters out the elements that could not meet the conditions. They return null 
;; when the input is null.
;;
;; Different: We have implemented the function "dropfalses" differently than the original which makes it slightly different
;; structured than the others. 
;;
;; Check your work with a member of the course staff!
;;
;; Once you know the essential difference between these three functions, create
;; the list-filter function that factors out this redundancy. list-filter
;; should behave indentically to the filter function when you are done!

;; (list-filter f lst): list?
;; f: boolean returning procedure?
;; lst: list?
;; It returns the filtered list of lst with the condition f such that
;; if condition f is true, it will remove the element, but if else,
;; it will not remove the element.
(define list-filter
  (lambda (f lst)
    (match lst
      [null null]
      [(cons head tail)
        (if (f head)
          (list-filter f tail)
          (cons head (list-filter f tail))
        )
      ]
    )
  )
)

(test-case "standard input" equal? (list #t #t #t #t) (list-filter not (list #t #t #f #f #f #t #f #t)))
(test-case "null input" equal? null (list-filter not null))
(test-case "length-less-than-five empty" equal? (list "abba" "doo!") (list-filter (lambda (x) (>= (string-length x) 5)) (list "abba" "yabba" "dabba" "doo!")))

;; -------------------
"Problem 3: Reduction"
;; -------------------

;; At this point, there's one function left to write from the big three---fold!
;; Let's, again, follow the same procedure: write some specific functions and
;; generalize from there. Here are two examples:

;; (sum-with-init result lst) -> number?
;;   result: number?
;;   lst: list? of numbers
;; Returns the sum of the numbers in lst, starting with result as the initial
;; value.
(define sum-with-init
  (lambda (result lst)
    (match lst
      [null result]
      [(cons head tail)
       (sum-with-init (+ head result) tail)])))

(test-case "sum-with-init empty"
           equal?
           22
           (sum-with-init 22 null))

(test-case "sum-with-init non-empty?"
           equal?
           50
           (sum-with-init 11 (list 27 2 10)))

;; (cons-onto-backwards result lst) -> list?
;;   result: lst?
;;   lst: list?
;; Returns the result of consing lst onto the front of result backwards.
(define cons-onto-backwards
  (lambda (result lst)
    (match lst
      [null result]
      [(cons head tail)
       (cons-onto-backwards (cons head result) tail)])))

(test-case "cons-onto-backwards empty"
           equal?
           (list 1 2 3)
           (cons-onto-backwards (list 1 2 3) null))

(test-case "cons-onto-backwards non-empty?"
           equal?
           (list 7 6 5 4 1 2 3)
           (cons-onto-backwards (list 1 2 3) (list 4 5 6 7)))

;; (Partner A drives!)
;;
;; Follow the style of these two functions to write a similar, third function
;; called string-append-backwards. (string-append-backwards result lst) takes
;; an initial string value, and a list of strings as input and returns the
;; strings of the list appended onto the front of the initial string in
;; backwards order. Note that the order of the individual characters in each
;; string is preserved, but they are appended in backwards order. For example:
;;
;; (string-append-backwards "abc" (list "def" "h" "gi"))
;; > "gihdefabc"
;; (string-append-backwards "abc" null)
;; > "abc"

;; (string-append-backwards result strL): string?
;; result: string?
;; strL: string list?
;; It returns each element of strL appended in front of 
;; result. It returns null when strL is null.
(define string-append-backwards
  (lambda (result strL)
    (match strL
        [null result]
        [(cons head tail)
        (string-append-backwards (string-append head result) tail)]
    )
  )
)
(test-case "stdin" equal? "gihdefabc" (string-append-backwards "abc" (list "def" "h" "gi")))
(test-case "nullin" equal? "abc" (string-append-backwards "abc" null))

;; (Partner B drives!)
;;
;; Like the previous problems, first identify what is shared and different
;; between these three functions:
;;
;; Shared: They all have the structure 
;;  (match {list}
;;      [null result]
;;      [(cons head tail)
;;      (string-append-backwards ({procedure} head result) tail)]
;;  ). Also, they apply one procedure with each of the element of the input list.
;;
;; Different: They utilize different functions to be applied to each element of the lst 
;; and the result.
;;
;; Again, check your work with a member of the course staff!
;;
;; Once you know the essential difference between these three functions, create
;; the list-foldl function that factors out this redundancy. list-foldl
;; should behave indentically to the foldl function when you are done!

;; (list-foldl f result lst): any?
;; f: procedure? (since result and each element of lst has to interact, 
;; it has to take in at least two inputs.)
;; result: any?
;; lst: list?
;; It inputs each of the elements of lst and result into the procedure f. It can return
;; anything depending on the procedure f.
(define list-foldl
  (lambda (f result lst)
    (match lst
        [null result]
        [(cons head tail)
        (list-foldl f (f head result) tail)]
    )
  )
)

(test-case "stdin" equal? "gihdefabc" (list-foldl string-append "abc" (list "def" "h" "gi")))
(test-case "list-foldl cons non-empty?" equal? (list 7 6 5 4 1 2 3) (list-foldl cons (list 1 2 3) (list 4 5 6 7)))

;; ----------------------------
"Problem 4: Really, Reductions"
;; ----------------------------

;; (Partner A drives!)
;;
;; In our discussion of list transformations, rather than foldl, we introduced
;; reduce first! Reduce is similar to foldl but instead of providing an initial
;; value, we use the first element of the list as the initial value.
;;
;; Implement list-reduce below in terms of list-foldl. It should be
;; functionality identical to reduce when you are done!

;; TODO: add documentation!
(define list-reduce
  (lambda (f l)
    null))

;; TODO: add tests here!

;; With implementations of list-foldl and list-reduce in hand, you should
;; be in a better position to now talk about when you would use foldl versus
;; reduce. Based on your implementation, give 2 reasons when you would choose
;; foldl versus reduce:
;;
;; <TODO: write down your three reasons here>
;; 1. ...
;; 2. ...
;; 3. ...

;; --------------------------
"Problem 5: That's Backwards"
;; --------------------------

;; (Partner B drives!)
;;
;; Awkwardly, foldl seems to "reverse" our computations. In our above examples
;; we saw that cons-onto and string-append operated in a backwards fashion
;; when implemented with foldl. Use your implementations to compare
;; how foldl operates between sum-on-init and string-append-backwards. Trace
;; through an example execution of each function to highlight these differences
;; below:
;;
;; (sum-with-init 11 (list 27 2 10)))
;; > (match (list 27 2 10) [null 11] [(cons head tail) (sum-with-init (+ 11 head) tail)])
;; > (match (list 27 2 10) [(cons head tail) (sum-with-init (+ 11 head) tail)])
;; > (sum-with-init (+ 11 27) (list 2 10))
;; > (sum-with-init 38 (list 2 10))
;; > (match (list 2 10) [null 38] [(cons head tail) (sum-with-init (+ 38 head) tail)])
;; > (match (list 2 10) [(cons head tail) (sum-with-init (+ 38 head) tail)])
;; > (sum-with-init (+ 38 2) (list 10))
;; > (sum-with-init 40 (list 10))
;; > (match (list 10) [null 40] [(cons head tail) (sum-with-init (+ 40 head) tail)])
;; > (match (list 10) [(cons head tail) (sum-with-init (+ 40 head) tail)])
;; > (sum-with-init (+ 40 10) null)
;; > (sum-with-init 50 null)
;; > (match null [null 50] [(cons head tail) (sum-with-init (+ 50 head) tail)])
;; > 50
;;
;; (string-append-backwards "abc" (list "def" "h" "gi"))
;; > (match (list "def" "h" "gi") [null "abc"] [(cons head tail) (string-append-backwards (string-append head "abc") tail)])
;; > (match (list "def" "h" "gi") [(cons head tail) (string-append-backwards (string-append head "abc") tail)])
;; > (string-append-backwards (string-append "def" "abc") (list "h" "gi"))
;; > (string-append-backwards "defabc" (list "h" "gi"))
;; > (match (list "h" "gi") [null "defabc"] [(cons head tail) (string-append-backwards (string-append head "defabc") tail)])
;; > (match (list "h" "gi") [(cons head tail) (string-append-backwards (string-append head "defabc") tail)])
;; > (string-append-backwards (string-append "h" "defabc") (list "gi"))
;; > (string-append-backwards "hdefabc" (list "gi"))
;; > (match (list "gi") [null "hdefabc"] [(cons head tail) (string-append-backwards (string-append head "hdefabc") tail)])
;; > (match (list "gi") [(cons head tail) (string-append-backwards (string-append head "hdefabc") tail)])
;; > (string-append-backwards (string-append "gi" "hdefabc") null)
;; > (string-append-backwards "gihdefabc" null)
;; > (match null [null "gihdefabc"] [(cons head tail) (string-append-backwards (string-append head "gihdefabc") tail)])
;; > "gihdefabc"
;;
;; In a sentence or two, explain why string-append-backwards performs its
;; "backwards" behavior but sum-with-init seems to work as expected.
;;
;; Since addition of the elements are communitive, so the order of the addition does not matter.
;; For the strings, however, the order matters, so it may not work under "apply" conditions.

;; foldl works through the elements of the list in a left-to-right fashion.
;; Counterintuitively, this results in backwards behavior! To get the desired
;; behavior for string-append, we need to go through the elements in
;; right-to-left fashion. This is a variant of fold call foldr!
;;
;; Implement list-foldr below which should behave functionally identically to
;; foldr when you are done. Implement this function using recursion without
;; appealing to any additional functions from the standard library, e.g.,
;; reverse.
;;
;; Note, in foldl, we assumed that the function behaved as follows:
;;
;; + The first argument is the accumulated result.
;; + The second argument is an element of the list.
;;
;; For foldr, we traditionally switch the order of arguments so the first
;; argument is the element from the list and the second is the accumulated
;; result. We'll see why this is useful shortly!
;;
;; (Hint: think about how you integrated the head of the list into the result
;; in foldl. To get the desired effect for foldr, you should integrate the
;; head into the result in the other possible way!

;; (list-foldr f lst): any?
;; f: procedure?(has to take in at least 2 inputs)
;; lst: list?
;; It uses the first element of lst as the initial input, and it accumulates the function
;; onto the first element.
(define list-foldr
  (lambda (f result lst)
    (match lst
      [null result]
      [(cons head tail) (list-foldr f (f result head) tail)]
    )
  )
)
(test-case "string-app" equal? "abcdefghi" (list-foldr string-append "abc" (list "def" "g" "hi")))
(test-case "string-null" equal? "abc" (list-foldr string-append "abc" (list)))

;; Finally, let's compare the behavior of foldl and foldr. If f is the
;; function we're folding over, init is the initial value, and
;; x1, ..., xk are the values of the list in l, there are two ways that
;; we can combine everything together to get the fold:
;;
;; f(f(f(f(f(init, x1), x2), x3), ...), xk)
;; foldr
;; since it starts with an initial case on the first input line of f, and proceeds to the right.
;; or
;;
;; f(x1, f(x2, f(x3, f(..., f(xk, init)))))
;; foldl
;; since it starts with an initial case on the second input line of f, and proceeds to the left.