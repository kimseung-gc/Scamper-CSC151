;; CSC-151-NN (F22)
;; Lab: Binary Trees (binary-trees.rkt)
;; Authors: Joyce Gill, Seunghyeon Kim
;; Date: November 9th, 2022
;; Acknowledgements:
;;   ACKNOWLEDGEMENTS HERE

; +-----------------------------------------+------------------------
; | Provided code: The definition of a tree |
; +-----------------------------------------+

(struct leaf ())
(struct node (value left right))
(define tree?
  (lambda (v) (or (leaf? v) (node? v))))

; +-----------------------------+------------------------------------
; | Provided code: tree->string |
; +-----------------------------+

(define tree-level->string
  (let ([bullets (vector "* " "+ " "- " ". ")]
        [make-spaces (lambda (n)
                       (list->string (make-list n #\space)))])
    (lambda (level tree)
      (let* ([spaces (make-spaces (* 2 level))]
             [bullet
               (string-append
                 spaces
                 (vector-ref bullets (remainder level (vector-length bullets))))])
        (match tree
          [(leaf) ""]
          [(node value (leaf) (leaf)) (string-append bullet value)]
          [(node value left (leaf))
           (string-append
             (string-append bullet value)
             "\n"
             (tree-level->string (+ level 1) left))]
          [(node value (leaf) right)
           (string-append
             (string-append bullet value)
             "\n"
             (tree-level->string (+ level 1) right))]
          [(node value left right)
          (string-append
            (string-append bullet value)
            "\n"
            (tree-level->string (+ 1 level) left)
            "\n"
            (tree-level->string (+ 1 level) right))])))))

(define tree->string
  (lambda (tree)
    (tree-level->string 0 tree)))

; +----------------------------------------------+-------------------
; | Provided code: The legendary management tree |
; +----------------------------------------------+

(define management-tree
  (node
    "Board"
    (leaf)
    (node
      "CEO"
      (node
        "Head of Engineering"
        (node "Software Developer" (leaf) (leaf))
        (node "Tester" (leaf) (leaf)))
      (node
        "Head of Legal"
        (leaf)
        (node "Lawyer" (leaf) (leaf))))))

(tree->string management-tree)

; +-----------+------------------------------------------------------
; | Exercises |
; +-----------+

; ------------------------
"Exercise 1: Making trees"
; ------------------------

; (Partner A drives!)

; a. Consider the following trees of numbers drawn with ASCII art:

; i. tree-i
;           "b"
;           / \
;          /   \
;        "a"   "c"

; ii. tree-ii
;            "e"
;            / \
;           /   \
;          "b"  "f"
;          / \    \
;         /   \    \
;       "a"   "c"  "g"
;             /
;            "d"

; iii. tree-iii
;           "f"
;           /
;         "e"
;         /
;       "d"
;       /
;     "c"
;     /
;   "b"
;   /
; "a"

; For each of these trees, use the tree-making functions from the
; reading to complete the definitions of `tree-i`, `tree-ii`, and
; `tree-iii` below. 

; Make sure to check that each thing you enter is a binary tree with
; `binary-tree?` and that it has the right form with `display-binary-tree`

;     > (tree? tree-i)
;     #t
;     > (tree->string tree-i)
;     * b
;       + a
;       + c
;     > (binary-tree? tree-ii)
;     #t
;     > (display-binary-tree tree-ii)
;     * 5
;       + 2
;         - 1
;         - 4
;       + 7
;         - 9
;           . 8
;     > (binary-tree? tree-iii)
;     #t(node-value (node-left tree-i))
;     > (display-binary-tree tree-iii)
;     * 5
;       + 4
;         - 3
;           . 2
;             . 1
;               . 0

(define tree-i
  (node
    "b"
    (node "a" (leaf) (leaf))
    (node "c" (leaf) (leaf))))

(tree->string tree-i)

(define tree-ii
  (node
    "e"
    (node "b" 
      (node "a" (leaf) (leaf)) 
      (node "c" 
        (node "d" (leaf) (leaf)) 
        (leaf)))
    (node "f" (leaf) (node "g" (leaf) (leaf)))))

(define tree-iii
  (node "f" (node "e" (node "d" (node "c" (node "b" (node "a" (leaf) (leaf)) (leaf)) (leaf)) (leaf)) (leaf)) (leaf)))

(tree->string tree-iii)
; b. Note that tree-iii is a left-leaning tree. That is, all its children are
; left children. Complete the definition of tree-iv below which is
; the same as tree-iii except that its leaves grow to the right rather
; than the left.

; "a"
;   \
;   "b"
;     \
;     "c"
;       \ 
;       "d"
;         \ 
;         "e"
;           \
;           "f"

; > (tree? tree-iv)
; #t
; > (tree->string tree-iv)
; * 5
;   + 4
;     - 3
;       . 2
;         * 1
;           + 0

(define tree-iv
  (node "a" (leaf) (node "b" (leaf) (node "c" (leaf) (node "d" (leaf) (node "e" (leaf) (node "f" (leaf) (leaf))))))))


; d. Finally, in the space below describe in a few sentences the
; differences and similarities between tree-iii and tree-iv. Do you
; consider these trees to be the same tree or different trees?
; Why?

; They are similar, but different since the root of the tree is different.

; ------------------
"Exercise 2: Leaves"
; ------------------

; (Partner B drives!)

; a. As you may have noted, in the sample code, we use the very verbose
;
; (node val (leaf) (leaf))
;
; To create a node with no children. This is a bit tedious! Write a
; helper function 'node-nc' (short for "no children") that takes a value
; as input and produces a node with no children as output. Document
; the function appropriately.

; (node-nc str): node?
; str: string?
; takes a value str and produces a node with no values

(define node-nc
  (lambda (str)
    (node str (leaf) (leaf))
  )
)

; Now, write a function 'childless?' that takes a tree as input and
; returns #t if and only if the tree has no children. Use the query
; functions produced by our struct declarations leaf? and node? in
; conjunction with operations over booleans for this task.  Document
; and test this function appropriately.

(define childless?
  (lambda (tree)
    (match tree
      [(node-nc _) #t]
      [else #f]
    )
  )
)
; ----------------------------
"Exercise 3: Traversing trees"
; ----------------------------

; (Partner C drives!)

; Recall from our discussion of structs that we can get out the
; fields of a struct in two ways:
;
; 1. Using struct projection functions.
; 2. Pattern matching.
;
; For our tree structs, we have:
;
; + The (leaf) pattern to match a leaf.
; + The (node-value n), (node-left n), and (node-right n) functions
;   to retrieve the value, left child, and right child of a node.
;   We also have the pattern (node value left right) to pattern
;   match a node and bind its value, left, and right fields all
;   at once.
;
; In the space below, write two expressions to retrieve the given
; value from the trees you created above:
;
; + One expression using combinations of projection functions.
; + One expression using a pattern matching consisting of a single
;   pattern. Recall that you can nest patterns inside of other
;   patterns.

; a. "b" from tree-i

(|> tree-i node-value)

; b. "c" from tree-ii

(|> tree-ii node-left node-right node-value)

; c. "b" from tree-iii

(|> tree-iii node-left node-left node-left node-left node-value)

; d. "Head of Legal" from management-tree

(define management-tree (node "Board" (leaf) (node "CEO" (node "Head of Engineering" (node "Software Developer" (leaf) (leaf)) (node "Tester" (leaf) (leaf))) (node "Head of Legal" (leaf) (node "Lawyer" (leaf) (leaf))))))

(|> management-tree node-right node-right node-value)

; e. "Software Developer" from management-tree

(|> management-tree node-right node-left node-left node-value)

; ------------------------------------
"Exercise 4: Exploring tree recursion"
; ------------------------------------

; (Partner D drives!)

; From the reading, we noted that a binary tree is recursively defined like a
; list. A binary tree is either:

; + *Empty*, or
; + *Non-empty* where the tree contains a value and up to two *children*
;   (*subtrees*) that are, themselves, trees.

; Like lists, our tree operations mirror this recursive decomposition of
; the list. As a first example, consider the following function which
; computes the *size* of the input tree, *i.e.*, the number of values it
; contains.

;;; (tree-size tree) -> integer?
;;;   tree : tree?
;;; Determine how many values are in binary tree.
(define tree-size
  (lambda (t)
    (match t
      [(leaf) 0]
      [(node _ l r) (+ 1 (tree-size l) (tree-size r))])))

; a. For reference, copy and paste your definitions from tree-i and
; tree-ii from a previous problem within this comment below:

#|
 (define tree-i
  (node
    "b"
    (node "a" (leaf) (leaf))
    (node "c" (leaf) (leaf))))


(define tree-ii
  (node
    "e"
    (node "b" 
      (node "a" (leaf) (leaf)) 
      (node "c" 
        (node "d" (leaf) (leaf)) 
        (leaf)))
    (node "f" (leaf) (node "g" (leaf) (leaf)))))

|#

; Now, use your mental model of computation to give an evaluation trace
; of the following expression in the space below. In your derivation,
; you may take the following short-cuts:
;
; + You may evaluate a recursive call to tree-size directly to the
;   branch of the pattern match that is selected.
; + You may elide the contents of the tree's children during evaluation.
;
; Make sure to check your work in the explorations pane when you are
; done!

; a. (tree-size tree-i)
; --> (tree-size tree-i)
; --> (match tree-i [(leaf) 0][(node _ l r) (+ 1 (tree-size l) (tree-size r))])
; --> (match tree-i #f [(node _ l r) (+ 1 (tree-size l) (tree-size r))])
; --> (match tree-i [(node _ l r) (+ 1 (tree-size l) (tree-size r))])
; --> (match tree-i [(node "b" (node "a" (leaf) (leaf)) (node "c" (leaf) (leaf))) (+ 1 (tree-size (node "a" (leaf) (leaf))) (tree-size (node "c" (leaf) (leaf))))])
; --> (+ 1 (tree-size (node "a" (leaf) (leaf))) (tree-size (node "c" (leaf) (leaf))))
; --> (+ 1 (+ 1 (tree-size (leaf)) (tree-size (leaf))) (tree-size (node "c" (leaf) (leaf))))
; --> (+ 1 (+ 1 0 0) (tree-size (node "c" (leaf) (leaf))))
; --> (+ 1 (+ 1 0 0) (+ 1 0 0))
; --> 3


; b. (tree-size tree-ii)
; --> (tree-size tree-ii)
; --> (match tree-ii [(leaf) 0][(node _ l r) (+ 1 (tree-size l) (tree-size r))])
; --> (match tree-ii #f [(node _ l r) (+ 1 (tree-size l) (tree-size r))])
; --> (match tree-ii [(node _ l r) (+ 1 (tree-size l) (tree-size r))])
; --> (match tree-ii [(node _ (node "b" (node "a" (leaf) (leaf)) (node "c" (node "d" (leaf) (leaf)) (leaf))) (node "f" (leaf) (node "g" (leaf) (leaf)))) (+ 1 (tree-size l) (tree-size r))])
; --> (match tree-ii [(node _ (node "b" (node "a" (leaf) (leaf)) (node "c" (node "d" (leaf) (leaf)) (leaf))) (node "f" (leaf) (node "g" (leaf) (leaf)))) (+ 1 (tree-size (node "b" (node "a" (leaf) (leaf))) (tree-size (node "f" (leaf) (node "g" (leaf) (leaf)))))])
; --> (+ 1 (tree-size (node "b" (node "a" (leaf) (leaf))) (tree-size (node "f" (leaf) (node "g" (leaf) (leaf))))
; --> (+ 1 (+ 1 (tree-size (leaf)) (tree-size (leaf))) (tree-size (node "f" (leaf) (node "g" (leaf) (leaf))))
; --> (+ 1 (+ 1 0 0) (tree-size (node "f" (leaf) (node "g" (leaf) (leaf))))
; --> (+ 1 (+ 1 0 0) (+ 1 (tree-size (leaf)) (tree-size (node "g" (leaf) (leaf)))))
; --> (+ 1 (+ 1 0 0) (+ 1 0 (tree-size (node "g" (leaf) (leaf)))))
; --> (+ 1 (+ 1 0 0) (+ 1 0 (+ 1 (tree-size (leaf)) (tree-size (leaf)))))
; --> (+ 1 (+ 1 0 0) (+ 1 0 (+ 1 0 0)))
; --> 4

; Fill out the following high-level description of `tree-size` in
; terms of the base and recursive cases of the function above.

; The size of a tree is:
; + 0 in the base case
; + 1 in the recursive case