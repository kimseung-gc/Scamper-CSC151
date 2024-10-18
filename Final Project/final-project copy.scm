;; CSC-151-03 (Fall 2022)
;; Mini-Project 6: Synthesizer
;; Seunghyeon Kim
;; 2022-10-06
;; ACKNOWLEDGEMENTS:
;; Waveforms lab, Manipulation Waveforms lab, and Digital Audio reading

(import audio)
(import music)
(import canvas)
(import html)

; (scalingFunc duration t): number?
; duration: number? (non-negative)
; t: number? (non-negative)
; It returns the scaling value of vector at t where 
; it can be used by multiplying each spcfcVecCalcFunc 
; values such that it can express the decaying nature
; of the note.

(define scalingFunc 
  (lambda (duration t)
    (expt 2 (/ (* 5 t) (* -1 duration))); function for decaying pattern as a linear funtion
  )
)

; (singleVectorFunc wvfrm sample-rate waveFrequency clipPeriod t): number?
; wvfrmL: list? (Consist of valid waveforms in string format)
; sample-rate: number? (non-negative)
; waveFrequency: number (non-negative, and it has to be the frequency of each function)
; duration: number? (non-negative)
; t: number? (non-negative)
; It is a helper function for spcfcVecCalcFuncHelper. As Scamper requires
; memory control, I had to use "vector-map" function. Thus, 
; this function can evaluate the note value for each t (vectors
; in the vector range).

(define singleVectorFunc
  (lambda (wvfrm sample-rate waveFrequency duration t)
    (let*
      (
        [wavePeriod (/ 1 waveFrequency)]; period for each wave
        [nWavesFromPrevClip (quotient t wavePeriod)]); number of waves passed at t from the previous clip
      (cond
        [(equal? wvfrm "square") 
          (if (< t (+ (* nWavesFromPrevClip wavePeriod) (* 0.5 wavePeriod)))
            -1
            1)]
        [(equal? wvfrm "sine") 
          (sin (* (/ (* 2 pi) wavePeriod) t))]
        [(equal? wvfrm "cosine")
          (cos (* (/ (* 2 pi) wavePeriod) t))]
        [(equal? wvfrm "triangle")
          (if (< t (+ (* nWavesFromPrevClip wavePeriod) (* 0.5 wavePeriod)))
            (- (* (/ 4 wavePeriod) (- t (* wavePeriod nWavesFromPrevClip))) 1)
            (- 3 (* (/ 4 wavePeriod) (- t (* wavePeriod nWavesFromPrevClip)))))]
        [(equal? wvfrm "sawtooth")
          (- (* (/ 2 wavePeriod) (- t (* wavePeriod nWavesFromPrevClip))) 1)]))))

; (spcfcVecCalcFunc wvfrmL sample-rate frequencyL duration t): number?
; wvfrmL: list? (consist of valid strings that expresses waveform types)
; sample-rate: number? (non-negative)
; frequencyL: list? (consist of numbers that express frequency for each waveform waves. 
; This must have same length as the wvfrmL)
; duration: number? (non-negative)
; t: number? (non-negative)
; It returns the vector value consist of multiple waveforms and frequencies.
; It can be used with vector-map and it can act as a helper function for 
; make-sample and make-sample-ADSR.

(define spcfcVecCalcFunc
  (lambda (wvfrmL sample-rate frequencyL maxAmpL duration t)
    (match wvfrmL
      [null 0]
      [(cons head tail) 
        (+ 
          (* (car maxAmpL) 
            (singleVectorFunc head sample-rate (car frequencyL) duration t)) 
            (spcfcVecCalcFunc tail sample-rate (cdr frequencyL) (cdr maxAmpL) duration t)
        )
      ]
    )
  )
)

; test case for the rest of the functions were not made since
; it will make the code significantly ugly, and test cases cannot
; compare sample-nodes.

; (make-sample wvfrmL sample-rate frequency duration): vector?
; wvfrmL: list? (Consist of valid waveforms in string format)
; sample-rate: number? (non-negative)
; frequencyL: list? (consist of non-netagive numbers)
; duration: number (non-negative)
; It returns notes of the same sample-rate, frequency, and duration
; with different waveforms which are assigned accordingly to wvfrmL.

(define make-sample
  (lambda (wvfrmL sample-rate frequencyL maxAmpL duration)
    (let*
      (
        [ttlSmpl (* sample-rate duration)]
        [singleVectorFuncMultipleWaveFormsSetUp (lambda (t) (* (spcfcVecCalcFunc wvfrmL sample-rate frequencyL maxAmpL duration t) (scalingFunc duration t)))]
        [vectorR (vector-range 0 duration (/ duration ttlSmpl))]
      )
      (vector-map singleVectorFuncMultipleWaveFormsSetUp vectorR)
    )
  )
)

; (make-sample-harp sample-rate frequency duration): vector?
; sample-rate: number? (non-negative)
; frequency: number? (non-netagive number)
; duration: number (non-negative)
; It returns the vector of a harp playing the specific frequency
; with the given duration.

(define make-sample-harp
  (lambda (sample-rate frequency duration)
    (let
      (
        [waveTypeL (list "sine" "sine" "cosine")]
        [frequencyCombination (list (* 3 frequency) frequency frequency)]
        [ampCombination (list (/ -1 4) (/ 1 4) (/ (expt 3 0.5) 2))]
      )
      (make-sample waveTypeL sample-rate frequencyCombination ampCombination duration)
    )
  )
)

; (parNotes-helper vectorL): vector?
; vectorL: list? (consists of the vector samples for each notes)
; It returns the vectors of all notes added together. It cannot scale
; on its own, so it is used as a helper function.

(define parNotes-helper
  (lambda (vectorL)
    (match vectorL
      [(cons head null) (vector-map + head)]
      [(cons head tail) (vector-map + head (parNotes-helper tail))]
    )
  )
)

; (parNotes vectorL): vector?
; vectorL: list? (consists of the vector samples for each notes)
; It returns the vectors of all notes added together, and scaled by the 
; number of notes. It has the same effect as playing the notes parallelly.

(define parNotes
  (lambda (vectorL)
    (vector-map (lambda (vec) (* (/ 1 (length vectorL)) vec)) (parNotes-helper vectorL))
  )
)

(define harpNt
  (lambda (MIDINt duration)
    (let*
      (
        [fixedSampleRate 21000]
        [freqV (vector 261.63 277.18 293.66 311.13 329.63 349.23 369.99 392.0 415.3 440.0 466.16 493.88)]
        [netFreq (- (quotient MIDINt 12) 5)]
        [freqInd (remainder MIDINt 12)]
        [freqScale (expt 2 netFreq)]
        [freq4 (vector-ref freqV freqInd)]
        [frequency (* freq4 freqScale)]
      )
      (make-sample-harp fixedSampleRate frequency duration)
    )
  )
)

;;; FRONT END

;; Creating canvas
(define cvs (canvas 462 300))

;; Creating vector that will alter which keys are pressed - ie. appear red on the canvas
(define keys-pressed (vector #f #f #f #f #f #f #f #f #f #f #f #f))

;; Procedures for pressing keys - Needed as seperate procedures instead of a single define function, as they need to be put through the (trigger)
;; function in order to be used in parallel with playing notes on the keyboard.
(define press-C (lambda () (vector-set! keys-pressed 0 #t)))
(define press-C# (lambda () (vector-set! keys-pressed 1 #t)))
(define press-D (lambda () (vector-set! keys-pressed 2 #t)))
(define press-D# (lambda () (vector-set! keys-pressed 3 #t)))
(define press-E (lambda () (vector-set! keys-pressed 4 #t)))
(define press-F (lambda () (vector-set! keys-pressed 5 #t)))
(define press-F# (lambda () (vector-set! keys-pressed 6 #t)))
(define press-G (lambda () (vector-set! keys-pressed 7 #t)))
(define press-G# (lambda () (vector-set! keys-pressed 8 #t)))
(define press-A (lambda () (vector-set! keys-pressed 9 #t)))
(define press-A# (lambda () (vector-set! keys-pressed 10 #t)))
(define press-B (lambda () (vector-set! keys-pressed 11 #t)))
(define release-all (lambda () (vector-fill! keys-pressed #f)))

;; Creating canvas function, x y represent the x and y coordinates on the canvas when pressed by a mouse-click. However after some experimentation,
;; the top-left corner of the canvas is not (0,0) but rather something akin to (20,80) and the numbers increase if you go right (on the x-axis) and
;; down (on the y-axis). Hence 20 to x and 80 to y was added to each 'predicted' position on the canvas to define which key was being played.
;; 
;; Furthermore, the function of (play-composition (par (note Midi Dur) (seq (trigger release-all) (trigger press-X))))) was used. (play-composition)
;; was needed to actually play the note, as otherwise it would just create/return a note object instead of playing it which was not useful. 
;;
;; In addition to this, the note needed to also be played along with triggering the two procedures release-all and press-X in sequence. This is
;; because the program needed to turn the key being pressed RED, and ONLY the key that was being pressed. Since 'which key is red?' was operated
;; and decided by a vector, and press-X and release-all changed that vector's values, but at least ONE key needed to be red to indicate what was
;; clicked on most recently, the (trigger) function was used. (As that function could fit into seq and par)
;;
;; In addition to trigger, release-all would be triggered first and press-X would then be triggered in sequence. This would ensure that the keys
;; all turn white before one turns red--however this is not noticable as it is a near-instantaneous procedure. Hence the full function would be
;; playing the note and also at the same time triggering a combined function that instantaneously turns all the keys white and turns ONE red.
(canvas-onclick cvs
  (lambda (x y)
    (begin
      (cond
        [(and (> x 71) (< x 101) (< y 230)) ;;C# (Add 20 to X, add 80 to Y)
          (play-composition (par (note 61 qn) (seq (trigger release-all) (trigger press-C#))))]
        [(and (> x 127) (< x 157) (< y 230)) ;;D#
          (play-composition (par (note 63 qn) (seq (trigger release-all) (trigger press-D#))))]
        [(and (> x 269) (< x 299) (< y 230)) ;;F#
          (play-composition (par (note 66 qn) (seq (trigger release-all) (trigger press-F#))))]
        [(and (> x 335) (< x 365) (< y 230)) ;;G#
          (play-composition (par (note 68 qn) (seq (trigger release-all) (trigger press-G#))))]
        [(and (> x 401) (< x 431) (< y 230)) ;;A#
          (play-composition (par (note 70 qn) (seq (trigger release-all) (trigger press-A#))))]
        [(< x 86) ;;C
          (play-composition (par (note 60 qn) (seq (trigger release-all) (trigger press-C))))]
        [(< x 152) ;;D
          (play-composition (par (note 62 qn) (seq (trigger release-all) (trigger press-D))))]
        [(< x 218) ;;E
          (play-composition (par (note 64 qn) (seq (trigger release-all) (trigger press-E))))]
        [(< x 284) ;;F
          (play-composition (par (note 65 qn) (seq (trigger release-all) (trigger press-F))))]
        [(< x 350) ;;G
          (play-composition (par (note 67 qn) (seq (trigger release-all) (trigger press-G))))]
        [(< x 416) ;;A
          (play-composition (par (note 69 qn) (seq (trigger release-all) (trigger press-A))))]
        [else ;;B
          (play-composition (par (note 71 qn) (seq (trigger release-all) (trigger press-B))))]
      )
    )
  )
)

;; Creating the animated canvas itself, white keys are drawn first so that black keys can overlap them and the look of a piano can be achieved
;; without having to do the needless work of combining two rectangles for every white key. Made it so every key corresponds to a position on
;; the vector (keys-pressed) and depending on that the key is red or white/black. This creates the effect of "when you press a key, it turns
;; red until you press the next key."
(animate-with
  (lambda (time)
    (begin
      ;; Canvas
      (rectangle cvs 0 0 462 300 "solid" "white")

      ;; White Keys
      (if (vector-ref keys-pressed 0) ;;C
        (rectangle cvs 0 0 66 300 "solid" "red")
        (rectangle cvs 0 0 66 300 "outline" "black")
      )
      
      (if (vector-ref keys-pressed 2) ;;D
        (rectangle cvs 66 0 66 300 "solid" "red")
        (rectangle cvs 66 0 66 300 "outline" "black")
      )

      (if (vector-ref keys-pressed 4) ;;E
        (rectangle cvs 132 0 66 300 "solid" "red")
        (rectangle cvs 132 0 66 300 "outline" "black")
      )

      (if (vector-ref keys-pressed 5) ;;F
        (rectangle cvs 198 0 66 300 "solid" "red")
        (rectangle cvs 198 0 66 300 "outline" "black")
      )

      (if (vector-ref keys-pressed 7) ;;G
        (rectangle cvs 264 0 66 300 "solid" "red")
        (rectangle cvs 264 0 66 300 "outline" "black")
      )

      (if (vector-ref keys-pressed 9) ;;A
        (rectangle cvs 330 0 66 300 "solid" "red")
        (rectangle cvs 330 0 66 300 "outline" "black")
      )

      (if (vector-ref keys-pressed 11) ;;B
        (rectangle cvs 396 0 66 300 "solid" "red")
        (rectangle cvs 396 0 66 300 "outline" "black")
      )

      ;; Black Keys
      (if (vector-ref keys-pressed 1) ;;C#
        (rectangle cvs 51 0 30 150 "solid" "red")
        (rectangle cvs 51 0 30 150 "solid" "black")
      )

      (if (vector-ref keys-pressed 3) ;;D#
        (rectangle cvs 117 0 30 150 "solid" "red")
        (rectangle cvs 117 0 30 150 "solid" "black")
      )

      (if (vector-ref keys-pressed 6) ;;F#
        (rectangle cvs 249 0 30 150 "solid" "red")
        (rectangle cvs 249 0 30 150 "solid" "black")
      )

      (if (vector-ref keys-pressed 8) ;;G#
        (rectangle cvs 315 0 30 150 "solid" "red")
        (rectangle cvs 315 0 30 150 "solid" "black")
      )

      (if (vector-ref keys-pressed 10) ;;A#
        (rectangle cvs 381 0 30 150 "solid" "red")
        (rectangle cvs 381 0 30 150 "solid" "black")
      )    
    )
  )
)

;;Running Canvas
cvs
