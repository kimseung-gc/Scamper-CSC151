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


;;; --------------Shuta's part----------------------

;;;(getiHeads lsts i) -> list?
;;; lsts: list<vector>?
;;; i: integer?
;;;Takes lists of vectors, and return list of ith element of each vectors. This is a helper function of noteStrToList.
(define getiHeads
  (lambda (lsts i)
    (match lsts
      [null null]
      [(cons head tail) 
        (if (<= (vector-length head) i)
            (getiHeads tail i)
            (cons (vector-ref head i) (getiHeads tail i)))])))


;;;(getHeads lsts i) -> list?
;;; lsts: list<vector>?
;;; i: integer?
;;;Takes lists of vectors, and return list of elements of each vectors. This is a helper function of noteStrToList.
(define getHeads
  (lambda (lsts i)
    (let ([maxlen (apply max (map (lambda(k)(vector-length k)) lsts))])
      (if (= i maxlen)
          null
          (cons (getiHeads lsts i)
                (getHeads lsts (+ i 1)))))))

;;;(StrToList noteStr)-> list?
;;; noteStr: string?
;;;Takes a string that is our unique music notation, and returns a list that is transposed. For example, "C D|A B" returns (list (list "C" "A") (list "D" "B")).
(define StrToList
  (lambda (noteStr)
    (|> noteStr
      (lambda(n)(string-split n "|"))
      (lambda(n)(map (lambda(n)(list->vector(string-split n " "))) n))
      (lambda(n)(getHeads n 0)))))

(StrToList "C D|A B")

;;;(noteToMIDI n) -> number? 
;;; n : string? note as string
;;;returns the MIDI value corresponding to that note.
(define noteToMIDI
    (lambda (n)
        (cond
            [(equal? n "C")     60]
            [(equal? n "C#")    61]
            [(equal? n "Db")    61]
            [(equal? n "D")     62]
            [(equal? n "D#")    63]
            [(equal? n "Eb")    63]
            [(equal? n "E")     64]
            [(equal? n "F")     65]
            [(equal? n "F#")    66]
            [(equal? n "Gb")    66]
            [(equal? n "G")     67]
            [(equal? n "G#")    68]
            [(equal? n "Ab")    68]
            [(equal? n "A")     69]
            [(equal? n "A#")    70]
            [(equal? n "Bb")    70]
            [(equal? n "B")     71]
            [else (error "Invalid pitch given: <n>")])))

;;;(inst note duration) -> vector?
;;; inst: string? (only "harp" is supported)
;;; note: string? (MIDI note)
;;; duration: number?
;;;Returns a note of specific instrument with MIDI value and duration.
(define stringNt
  (lambda (inst note duration)
      (cond	[(equal? inst "harp") (harpNt (noteToMIDI note) duration)])))

;;;(instSampParallel inst notes durations) -> list?
;;; inst: string? (only "harp" is supported)
;;; notes: List? 
;;; durations: List?
;;; duration: number?
;;;Makes a list of vectors played in parallel for input to the parNotes function.
(define instSampParallel
  (lambda (inst notes durations)
    (match notes
      [null null]
      [(cons head tail) (cons (stringNt inst head (string->number(car durations)))
                              (instSampParallel inst tail (cdr durations)))])))

;;;(instSampHelp inst notes durations) -> vector?
;;; inst: string? (only "harp" is supported)
;;; notes: List<list?>? (2d list, has to be converted with StrToList function)
;;; durations: List<list?>? (2d list, has to be converted with StrToList function)
;;; duration: number?
;;;This is helper function of instrumentSample to do recursion.
(define instSampHelp
  (lambda (inst notes durations)
      (match notes
        [(cons head null) (parNotes (instSampParallel inst (car notes) (car durations)))]
        [(cons head tail)(vector-append (parNotes (instSampParallel inst (car notes) (car durations)))
                                        (instSampHelp inst (cdr notes) (cdr durations)))])))

;;;(instrumentSample inst noteStr durationStr) -> vector?
;;; inst: string? (only "harp" is supported)
;;; noteStr: string? (our notation, "C D|A E" is supported. Each sequence of sounds are separated by '|'. Sequence of sounds are written with MIDI-character with single space between two sounds. In this case, "C" and "A" are played together first and then "D" and "E" are played.)
;;; duration: number?
;;;Returns sound information based on music score given by string. Only harp is supported now.
(define instrumentSample
  (lambda (inst noteStr durationStr)
    (let ([notes (StrToList noteStr)]
          [durations (StrToList durationStr)])
    (instSampHelp inst notes durations))))

;;; --------------Shuta's part end----------------------


;;; FRONT END

(define cvs (canvas 462 300))

(define keys-pressed (vector #f #f #f #f #f #f #f #f #f #f #f #f))

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

(animate-with
  (lambda (time)
    (begin
      ;; Canvas
      (rectangle cvs 0 0 462 300 "solid" "white")

      ;; White Keys
      (if (vector-ref keys-pressed 0)
        (rectangle cvs 0 0 66 300 "solid" "red")
        (rectangle cvs 0 0 66 300 "outline" "black")
      )
      
      (if (vector-ref keys-pressed 2)
        (rectangle cvs 66 0 66 300 "solid" "red")
        (rectangle cvs 66 0 66 300 "outline" "black")
      )

      (if (vector-ref keys-pressed 4)
        (rectangle cvs 132 0 66 300 "solid" "red")
        (rectangle cvs 132 0 66 300 "outline" "black")
      )

      (if (vector-ref keys-pressed 5)
        (rectangle cvs 198 0 66 300 "solid" "red")
        (rectangle cvs 198 0 66 300 "outline" "black")
      )

      (if (vector-ref keys-pressed 7)
        (rectangle cvs 264 0 66 300 "solid" "red")
        (rectangle cvs 264 0 66 300 "outline" "black")
      )

      (if (vector-ref keys-pressed 9)
        (rectangle cvs 330 0 66 300 "solid" "red")
        (rectangle cvs 330 0 66 300 "outline" "black")
      )

      (if (vector-ref keys-pressed 11)
        (rectangle cvs 396 0 66 300 "solid" "red")
        (rectangle cvs 396 0 66 300 "outline" "black")
      )

      ;; Black Keys
      (if (vector-ref keys-pressed 1)
        (rectangle cvs 51 0 30 150 "solid" "red")
        (rectangle cvs 51 0 30 150 "solid" "black")
      )

      (if (vector-ref keys-pressed 3)
        (rectangle cvs 117 0 30 150 "solid" "red")
        (rectangle cvs 117 0 30 150 "solid" "black")
      )

      (if (vector-ref keys-pressed 6)
        (rectangle cvs 249 0 30 150 "solid" "red")
        (rectangle cvs 249 0 30 150 "solid" "black")
      )

      (if (vector-ref keys-pressed 8)
        (rectangle cvs 315 0 30 150 "solid" "red")
        (rectangle cvs 315 0 30 150 "solid" "black")
      )

      (if (vector-ref keys-pressed 10)
        (rectangle cvs 381 0 30 150 "solid" "red")
        (rectangle cvs 381 0 30 150 "solid" "black")
      )    
    )
  )
)

(sample-node(instrumentSample "harp" "C D|A A" "1 1|1 1"))
cvs