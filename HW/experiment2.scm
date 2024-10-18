(import audio)

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

; (singleVectorFuncNnt wvfrm sample-rate minorFrequency clipPeriod t): number?
; wvfrm: string? in a valid format with the type of wave.
; sample-rate: number? (non-negative)
; minorFrequency: number (non-negative, and it has to be the frequency of each function)
; clipPeriod: number? (non-negative)
; t: number? (non-negative)
; It is a helper function for make-sample. As Scamper requires
; memory control, I had to use "vector-map" function. Thus, 
; this function can evaluate the note value for each t.

(define singleVectorFuncSynthNts
  (lambda (wvfrm sample-rate waveFrequency clipPeriod t)
    (let*
      (
        [nClip (quotient t clipPeriod)]; number of clips passed at the point of t
        [tClip (remainder t clipPeriod)]; time passed from the previouse clip
        [wavePeriod (/ 1 waveFrequency)]; period for each wave
        [nWavesFromPrevClip (quotient tClip wavePeriod)]
        [scalingFunc (- (+ 1 nClip) (/ t clipPeriod))])
      (cond
        [(equal? wvfrm "square") 
          (if (< tClip (+ (* nWavesFromPrevClip wavePeriod) (* 0.5 wavePeriod)))
            (* -1 scalingFunc)
            scalingFunc)]
        [(equal? wvfrm "sine") 
          (let
            (
              [pi 3.14159265]) 
            (* (sin (* (/ (* 2 pi) wavePeriod) tClip)) scalingFunc))]
        [(equal? wvfrm "triangle")
          (if (< tClip (+ (* nWavesFromPrevClip wavePeriod) (* 0.5 wavePeriod)))
            (* (- (* (/ 4 wavePeriod) (- tClip (* wavePeriod nWavesFromPrevClip))) 1) scalingFunc)
            (* (- 3 (* (/ 4 wavePeriod) (- tClip (* wavePeriod nWavesFromPrevClip)))) scalingFunc))]
        [(equal? wvfrm "sawtooth")
          (* scalingFunc (- (* (/ 2 wavePeriod) (- tClip (* wavePeriod nWavesFromPrevClip))) 1))
        ]))))

; (synthesize-notes wvfrm sample-rate frequency duration n): vector?
; wvfrm: string? in a valid format with the type of wave.
; sample-rate: number? (non-negative)
; frequency: number? (nun-netagive)
; duration: number (non-negative)
; n: integer? (non-negative)
; It returns n notes of the same sample-rate, frequency, and duration
; with same waveforms.

(define synthesize-notes
  (lambda (wvfrm sample-rate frequency duration n)
    (let*
      (
        [ttlSmpl (* sample-rate duration)]
        [clipPeriod (/ duration n)]
        [singleVectorFuncSynthNtsSetUp (lambda (t) (singleVectorFuncSynthNts wvfrm sample-rate frequency clipPeriod t))]
        [vectorR (vector-range 0 duration (/ duration ttlSmpl))]
      )
      (vector-map singleVectorFuncSynthNtsSetUp vectorR)
    )
  )
)
