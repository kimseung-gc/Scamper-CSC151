(import music)
;;; (func-4 l): pair?
;;; l: list?
;;; It returns the list of index i and index i+1, where i is even, switched.
;;; If the last index is odd, then the element remains.

(define func-4
  (lambda (l)
    (if (< (length l) 2)
        l
        (let ([h1 (car l)]
              [h2 (car (cdr l))]
              [t (cdr (cdr l))])
          (cons h2 (cons h1 (func-4 t)))))))

(func-4 (list 50 100 20 10 30 60 15))
(func-4 (list 50 "iei" 20 10 "ee0" 60))
(func-4 "iei")

; +----------------------------------------------------------------------------- 
; + Problem 4: Chords, Oh My! |
; +----------------------------

"Problem 4: Chords, Oh My!"
"========================="

; (B drives for this problem)

; Using musical intervals, we can characterize the various _chords_, collections
; of tones played simultaneously. A list of common chord types can be found on
; Wikipedia:
;
; https://en.wikipedia.org/wiki/Chord_(music)#Examples
;
; The table found on Wikipedia describes for each common chord its components
; in terms of intervals. For example:
;
; + A major triad is composed of the root (P1) and then the note 4 semitones
;   away (major third or M3) and the note 7 semitones away (perfect fifth or
;   P5).
; + A minor seventh chord is composed of the root (P1), the note 3 semitones
;   away (minor third, m3), the note 7 semitones away (P5), and the note 10
;   semitones away (minor seventh, m7).
;
; Rather than expressing intervals in terms of semitones, it is much more
; natural to express intervals in terms of the shorthand names given in the
; table, e.g., P4 (perfect fourth) corresponds to five semitones.
;
; First, write a function `(interval->note root int)` that takes the MIDI
; note value of the root and a named interval (as a string) and returns the
; MIDI note value of that interval note. For example,
; (interval->note 60 "P4") -->* 65 since a perfect fourth (P4) is 5 semitones
; away from the root. If the user provides a named interval that does not
; exist, then interval->note should raise a runtime error with the `error`
; function.
;
;;; (interval->note root interval): int?
;;; root: int?
;;; interval: string? (Has to be either "P1", "m2", "M2", "m3", "M3", "P4", "d5", "P5", "A5", "M6" or "d7", "m7", or "M7")
;;; It returns the MIDI value of the notes with the intervals.
;;; Their table values are written in https://en.wikipedia.org/wiki/Chord_(music)#Examples.

(define interval->note 
  (lambda (root interval)
    (cond
      [(string=? interval "P1") (+ root 0)]
      [(string=? interval "m2") (+ root 1)]
      [(string=? interval "M2") (+ root 2)]
      [(string=? interval "m3") (+ root 3)]
      [(string=? interval "M3") (+ root 4)]
      [(string=? interval "P4") (+ root 5)]
      [(string=? interval "d5") (+ root 6)]
      [(string=? interval "P5") (+ root 7)]
      [(string=? interval "A5") (+ root 8)]
      [(or (string=? interval "M6")(string=? interval "d7")) (+ root 9)]
      [(string=? interval "m7") (+ root 10)]
      [(string=? interval "M7") (+ root 11)]
      [else (error "Invalid interval! Please try again. [invalid input]")]
    )
  )
)

(interval->note 50 "P5")
(interval->note "P5" 50)
(interval->note 0 "M7")

; With interval->note, define a function `(triad root i1 i2 i3 dur appregio?)`
; that takes a MIDI note value as the root, three named intervals, a duration
; and a boolean, and creates a composition that plays a chord consisting of
; those three intervals played together. If appregio? is #f, then the notes
; of the chord are played in parallel. If appregio? is #t, then the notes
; are played separately, or appregiated, in sequence.

; As before, make sure to document your function and test it on a variety
; of inputs. Once you are done, use your function to play three chords from
; the table of common chords found on Wikipedia. Like the intervals, you
; should find that particular chord types evoke certain feelings or emotions!
; For each of the three chords you build, you should list in a comment below
; the one word that you feel when hearing that chord.

;;; (interval->note root i1 i2 i3 dur appregio?): int?
;;; root: int?
;;; i1, i2, i3: string? (Has to be either "P1", "m2", "M2", "m3", "M3", "P4", "d5", "P5", "A5", "M6" or "d7", "m7", or "M7")
;;; dur: dur?
;;; appregio?: boolean?
;;; It returns the composition of the note with i1, i2, i3 interval changes. 
;;; They could be set as parallel sequence or series by setting the "appregio?"
;;; variable (#t is series and #f is parallel).
;;; Their table values are written in https://en.wikipedia.org/wiki/Chord_(music)#Examples.

(define triad
  (lambda (root i1 i2 i3 dur appregio?) 
    (if appregio?
      (seq
        (note (interval->note root i1) dur)
        (note (interval->note root i2) dur)
        (note (interval->note root i3) dur)
      )
      (par 
        (note (interval->note root i1) dur)
        (note (interval->note root i2) dur)
        (note (interval->note root i3) dur)
      )
    )
  )
)

(triad 60 "M3" "P5" "M6" wn #t)
(triad 60 "M3" "P5" "M6" wn #f)
(triad 60 "P5" "M3" "m7" wn #f)

; These are each Cmaj6(series), Cmaj6(parallel), and Cdom7(parallel).
; Usually majors have brighter sounds than minors making the mood
; of a song happier.