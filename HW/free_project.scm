(import image)

(define func
  (lambda (x) 
    x
  )
)

(define maxpix 500)

(define scale 0.5)

(define assembly
    (lambda (x) (
        (if (< (func (* x scale)) (* maxpix scale))
            (above 
            (rectangle 1 (- (- maxpix 3) (/ (func (* x scale)) scale)) "solid" "white")
            (rectangle 1 1 "solid" "black") 
            (rectangle 1 (/ (func (* x scale)) scale) "solid" "white")  
            (rectangle 1 1 "solid" "black"))
            (above 
            (rectangle 1 (- maxpix 2) "solid" "white")
            (rectangle 1 1 "solid" "black")))
        )
    )
)

(define graph
    (lambda (x) (
        (if (zero? x)
            (beside (rectangle 1 maxpix "solid" "black") (graph (+ x 1)))
            (if (<= x maxpix)
                (beside (assembly x) (graph (+ 1 x)))
                (rectangle 1 maxpix "solid" "white")
            )
        )
    )
    )
)
(graph 0)

