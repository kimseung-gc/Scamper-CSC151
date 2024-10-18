; Structs

(struct student
  (name id address)
)

(define students
  (list 
    (student "Joe" 1234 #f) 
    (student "Jerry" #f "721 8th Ave")
    (student #f #f "854 Park Street")
    (student "Jim" 8492 "457 7th Street")))

(define replaceFalse
  (lambda (type var)
    (cond
      [(equal? type "id")
        (if (equal? #f var)
          -1
          var
        )
      ]
      [else 
        (if (equal? #f var)
          "unknown"
          var
        )
      ]
    )
  )
)

(define fillIn
  (lambda (studL)
    (match studL
      [null null]
      [(cons head tail)
        (match head
          [(student nm id add) 
            (cons 
              (student (replaceFalse "nm" nm) (replaceFalse "id" id) (replaceFalse "add" add)) 
              (fillIn tail))]
        )
      ]
    )
  )
)

(fillIn students)

; trees

(struct node
  (head left right)
)

(struct leaf
  ()
)

(define tree1
  (node "" 
    (node "" 
      (node "" 
        (node "" 
          (leaf) 
          (node "D" 
            (node "E" 
              (leaf) 
              (leaf)) 
            (node "F" 
              (leaf) 
              (leaf)))) 
        (node ""
          (leaf) 
          (leaf))) 
      (node "A" 
        (leaf) 
        (leaf))) 
    (node ""
      (leaf)
      (node ""
        (node ""
          (leaf)
          (leaf))
        (node ""
          (node "B"
            (node "C" 
              (leaf) 
              (node ""
                (leaf)
                (leaf)))
            (leaf))
          (leaf)))))
)

(define index-tree-helper
  (lambda (prev tree)
    (match tree
      [(node head (leaf) (leaf)) (leaf)]
      [(node head left (leaf)) (node prev (index-tree-helper (string-append prev "L") left) (leaf))]
      [(node head (leaf) right) (node prev (leaf) (index-tree-helper (string-append prev "R") right))]
      [(node head left right) (node prev (index-tree-helper (string-append prev "L") left) (index-tree-helper (string-append prev "R") right))]
    )
  )
)

(define index-tree
  (lambda (tree)
    (index-tree-helper "" tree)
  )
)

(index-tree tree1)

(define headT
  (lambda (tree)
    (match tree
      [(node head _ _) head]
    )))

(define leftT
  (lambda (tree)
    (match tree
      [(node _ left _) left]
    )))

(define rightT
  (lambda (tree)
    (match tree
      [(node _ _ right) right]
    )))

(define find-tree-helper
  (lambda (letter tree indT)
    (let
      (
        [LInd (leftT indT)]
        [RInd (rightT indT)]
        [HInd (headT indT)]
      )
      (match tree
        [(node head (leaf) (leaf)) 
          (if (equal? head letter)
            HInd
            "none"
          )
        ]
        [(node left (leaf)) 
          (if (equal? letter ))]
        []
        []
      )
    )
  )
)

(define find-tree
  (lambda (letter tree)
    (find-tree-helper letter tree (index-tree tree1))
  )
)

; Vectors

(define vector-filter2-helper
  (lambda (n f vec)
    (let
      (
        [curEl (vector-ref vec n)]
      )
      (if (zero? n)
        (if (f curEl)
          (vector-append (vector curEl))
          (vector-append (vector))
        )
        (if (f curEl)
          (vector-append (vector-filter2-helper (- n 1) f vec) (vector curEl))
          (vector-append (vector-filter2-helper (- n 1) f vec) (vector))
        )
      )
    )
  )
)

(define vector-filter2
  (lambda (f vec)
    (vector-filter2-helper (- (vector-length vec) 1) f vec)
  )
)

(vector-filter2 (lambda (x) (not (zero? x))) (vector 0 2 1 3 2 0))