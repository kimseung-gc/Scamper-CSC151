;; CSC-151-03 (Fall 2022)
;; Mini-Project 6: Synthesizer
;; Seunghyeon Kim
;; 2022-10-06
;; ACKNOWLEDGEMENTS:
;; Waveforms lab and 

(import audio)

(define square-sample
  (lambda (sample-rate frequency duration)
    (let*
      (
        [totalSmpls (* sample-rate duration)]
        [period (/ 1 frequency)]
        [vecRng (vector-range 0 duration (/ duration totalSmpls))]
        [Y-val (vector-map 
          (lambda (v) 
            (let 
              (
                [n (quotient v period)]) 
            (if (< v (+ (* n period) (* 0.5 period)))
              -1
              1
            ))) vecRng)]
      )
      Y-val)))

(define sawtooth-sample
  (lambda (sample-rate frequency duration)
    (let*
      (
        [totalSmpls (* sample-rate duration)]
        [period (/ 1 frequency)]
        [vecRng (vector-range 0 duration (/ duration totalSmpls))]
        [Y-val (vector-map 
          (lambda (v) 
            (let*
              (
                [n (quotient v period)]
                [t (- v (* period n))]) 
              (- (* (/ 2 period) t) 1))) vecRng)]
      )
      Y-val)))

(define triangle-sample
  (lambda (sample-rate frequency duration)
    (let*
      (
        [totalSmpls (* sample-rate duration)]
        [period (/ 1 frequency)]
        [vecRng (vector-range 0 duration (/ duration totalSmpls))]
        [Y-val (vector-map 
          (lambda (v) 
            (let* 
              (
                [n (quotient v period)]
                [t (- v (* period n))]) 
            (if (< v (+ (* n period) (* 0.5 period)))
              (- (* (/ 4 period) t) 1)
              (- 3 (* (/ 4 period) t))
            ))) vecRng)]
      )
      Y-val)))

(define sine-sample
  (lambda (sample-rate frequency duration)
    (let*
      (
        [totalSmpls (* sample-rate duration)]
        [period (/ 1 frequency)]
        [vecRng (vector-range 0 duration (/ duration totalSmpls))]
        [Y-val (vector-map 
          (lambda (v) 
            (let
              (
                [pi 3.14159265]) 
              (sin (* (/ (* 2 pi) period) v)))) vecRng)]
      )
      Y-val)))

;; Next, we need your definition of simple-envelope from the reading
;; question for the day. You can copy it in the space below. As a
;; reminder, (simple-envelope n) returns a simple, linearly decaying
;; envelope consisting of n samples.

(define simple-envelop
   (lambda (total-samples)
      (list->vector (reverse (range 0 1 (/ 1 total-samples))))
   )
)

;;; (apply-envelope clip envelope) -> vector?
;;;   clip: vector? of samples [-1.0, 1.0]
;;;   envelope: vector? of samples in the range [0, 1]
;;; Returns a new vector of samples where the envelope is applied to the clip.
(define apply-envelope
  (lambda (clip envelope)
    (vector-map (lambda (x y) (* x y)) clip envelope)))

;;; (synthesize-note waveform sample-rate frequency duration) -> vector?
;;;   waveform: string? one of "square", "sawtooth", "triangle", or "sine"
;;;   sample-rate: number?, a non-negative integer
;;;   frequency: number?, a non-negative number
;;;   duration: number?, a non-negative number
;;; Returns a vector of samples representing a single note syntheiszed from
;;; the given parameters.
(define synthesize-note
  (lambda (waveform sample-rate frequency duration)
    (cond
      [(equal? waveform "square") (apply-envelope (square-sample sample-rate frequency duration) (simple-envelop (* sample-rate duration)))]
      [(equal? waveform "sawtooth") (apply-envelope (sawtooth-sample sample-rate frequency duration) (simple-envelop (* sample-rate duration)))]
      [(equal? waveform "triangle") (apply-envelope (triangle-sample sample-rate frequency duration) (simple-envelop (* sample-rate duration)))]
      [(equal? waveform "sine") (apply-envelope (sine-sample sample-rate frequency duration) (simple-envelop (* sample-rate duration)))]
    )))

(define index->simple-envelope
    (lambda (x samples)
    (+ 1 (* x (/ -1 samples)))))

;;; (synthesize-notes waveform sample-rate frequency duration n) -> vector?
;;;   waveform: string? one of "square", "sawtooth", "triangle", or "sine"
;;;   sample-rate: number?, a non-negative integer
;;;   frequency: number?, a non-negative number
;;;   duration: number?, a non-negative number
;;;   n: number?, a non-negative integer
;;; Returns a vector of samples representing n note syntheiszed from the given
;;; parameters.

(define synthesize-notes
  (lambda (waveform sample-rate frequency duration n)
  (let* ([samples (* sample-rate duration)]
        [clip (cond 
            [(string=? waveform "square")
              (square-sample sample-rate frequency duration)]
            [(string=? waveform "sine")
              (sine-sample sample-rate frequency duration)]
            [(string=? waveform "triangle")
              (triangle-sample sample-rate frequency duration)]
            [(string=? waveform "sawtooth")
              (sawtooth-sample sample-rate frequency duration)])]  
        [env-ref (vector-range 0 samples)]
        [full-env (
          |> env-ref
          (lambda (vec)
                  (vector-map (lambda (e) (index->simple-envelope (remainder e (/ samples n))
                    (/ samples n))) vec)))])
        (apply-envelope clip full-env))))

(sample-node (synthesize-notes "sine" 16000 880 2 10))

(define f_n&freq
  (lambda (wvfrmL sample-rate minorFrequency t_p t)
    (let*
      (
        [n_1 (quotient t t_p)]
        [wvfrm (list-ref wvfrmL n_1)]
        [tPrime (remainder t t_p)]
        [t_p_p (/ sample-rate minorFrequency)]
        [n_2 (quotient tPrime t_p_p)]
        [majFreqFunc (- n_1 (/ t t_p))]
      )
      (cond
        [(equal? wvfrm "square") 
          (if (< tPrime (+ (* n_2 t_p_p) (* 0.5 t_p_p)))
            (* -1 majFreqFunc)
            majFreqFunc
          )
        ]
        [(equal? wvfrm "sine") 
          (let
            (
              [pi 3.14159265])
            (* majFreqFunc (sin (* (/ (* 2 pi) t_p_p) tPrime))))
        ]
        [(equal? wvfrm "triangle") 
          (if (< tPrime (+ (* n_2 t_p_p) (* 0.5 t_p_p)))
            (* majFreqFunc (- (* (/ 4 t_p_p) tPrime) 1))
            (* majFreqFunc (- 3 (* (/ 4 t_p_p) tPrime)))
          )
        ]
        [(equal? wvfrm "sawtooth") 
          (* majFreqFunc (- (* (/ 2 t_p_p) tPrime) 1))
        ]
      )
    )
  )
)

(define make-sample
  (lambda (waveFormL sample-rate frequency duration)
    (let*
      (
        [totalSmpls (* sample-rate duration)]
        [t_p (/ duration (length waveFormL))]
        [ind (lambda (s) (quotient s t_p))]
        [vectorL (vector-range 0 duration (/ duration totalSmpls))]
        [funcP (lambda (t) (f_n&freq waveFormL sample-rate frequency t_p t))]
      )
      (vector-map funcP vectorL)
    )
  )
)
(sample-node (make-sample (list "sine" "square") 12000 3000 1))