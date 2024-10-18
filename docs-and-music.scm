;; CSC 151 (SEMESTER)
;; Lab: Docs and Music (docs-and-music.scm)
;; Authors: YOUR NAMES HERE
;; Date: THE DATE HERE
;; Acknowledgements:
;;   ACKNOWLEDGEMENTS HERE

(import image)
(import music)

; +----------------------------------------------------------------------------- 
; + Problem 1: It's a Mystery |
; +----------------------------

"Problem 1: It's a Mystery"
"========================="

; (A and B sides switch driver-navigator roles for each function.)

; For each of the following functions:
;
; (a) Look up their documentation in the Scamper sidebar menu in VSCode.
; (b) Write down at least three test cases exercising the function on a variety
;     of inputs.
; (c) List the preconditions on the parameters of the function.
; (d) In a sentence or two, describe the postcondition of the function in your
;     own words.

; (path ...) from the image library

; (with-dashes ...) from the image library

; +----------------------------------------------------------------------------- 
; + Problem 2: What's up, Doc? |
; +-----------------------------

"Problem 2: What's up Doc?"
"========================="

; (A and B sides switch driver-navigator roles for each function.)

; Consider each of the following function definitions that are undocumented
; and poorly named. Note that these functions use techniques and libraries that
; we may not have introduced or used yet; that is fine! The purpose of this
; exercise is to get us to think about the _contract_ of these functions rather
; than how they work precisely.

;;; TODO: add a doc comment for func-1 here
(define func-1
  (lambda (n r e g b)
    (if (zero? n)
        (triangle 0 "outline" "black")
        (overlay (triangle (* n r) "outline" (color e g b 1))
                 (func-1 (- n 1) r e g b)))))

; TODO: add func-1 test cases here:
"----- func-1 tests -----"

;;; TODO: add a doc comment for func-2 here
(define func-2
  (lambda (s n)
    (string-append (substring s n (string-length s))
                   (substring s 0 n))))

; TODO: add func-2 test cases here:
"----- func-2 tests -----"

;;; TODO: add a doc comment for func-3 here
(define func-3
  (lambda (s k)
    (string-map (lambda (c)
                  (if (char-numeric? c) k c))
                s)))

; TODO: add func-3 test cases here:
"----- func-3 tests -----"

;;; TODO: add a doc comment for func-4 here
(define func-4
  (lambda (l)
    (if (< (length l) 2)
        l
        (let ([h1 (car l)]
              [h2 (car (cdr l))]
              [t (cdr (cdr l))])
          (cons h2 (cons h1 (func-4 t)))))))

; TODO: add func-4 test cases here:
"----- func-4 tests -----"

; For each function:
;
; (a) Explore how the function works by writing down 3--5 test cases
;     illustrating its behavior on a variety of inputs. You can determine
;     what types of inputs the program expects by inspecting the code or
;     by trying out different values to see if they work.
; (b) Based on your results, write down a doc comment for each function
;     as described in the reading. Make sure to include a function
;     signature describing any preconditions on the function's parameters
;     (e.g., their types) as well as postconditions on the output of
;     the function.

; +----------------------------------------------------------------------------- 
; + Problem 3: Interval Theory |
; +-----------------------------

"Problem 3: Interval Theory"
"=========================="

; (A drives for this problem)

; Let's change gears and return back to the music library that we introduced in
; mini-project 1. Recall that the music library exposes the following functions:
;
; (note midi-note dur)
; (seq c1 ... ck)
; (par c1 ... ck)
;
; That forms musical compositions by composing together individual notes
; sequentially (seq) or in parallel (par).
;
; In this problem, we'll explore some basic music theory: the relationship
; between notes. Suppose that we pick a note that we'll call the _root_. Then we
; can explore the relationship between the root and other notes. In western music
; theory, we recognize the _semitone_ as the smallest interval, or distance,
; between notes. With our MIDI notes, the musical distance between two MIDI
; notes, e.g., 60 and 61, is precisely 1 semitone. A list of the standard
; intervals can be found on Wikipedia:
;
; https://en.wikipedia.org/wiki/Interval_(music)
;
; Observe that there are 13 total intervals, corresponding to the notes that
; are between 0--12 semitones away from the root. The "0th interval" is the
; same note as the root while the note 12 semitones away is the same note but
; one _octave_ higher than the original note.
;
; Write a function (all-intervals root d) that takes a note value root and
; duration d as input and plays all the intervals of root from 0--12 semitones
; upwards. Make sure to:
;
; (a) Document your function including any relevant preconditions and
;     postconditions.
; (b) Use auxiliary defines and lets to minimize redundancy in your function.
;
; Once you are done, use your function to exercise your ear! Try your function
; on a few inputs to get a sense of what the different intervals sound like.
; You should observe that intervals sound more dissonant or harmonious than
; others. Some intervals might even invoke particular emotions or feelings!
;
; For each of the 13 intervals, in the space below, give a single word (or pair
; of words if you don't agree with your partner!) that captures how each
; interval sounds to your ears. Identifying such "flavors" to the intervals is
; the primary way that musicians build up their ability to recognize them in
; live music!
;
; <TODO: list your single-word descriptions for each of the 13 intervals here>
; ...

;; TODO: write your definition of all-intervals here

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
; Make sure to document your function and test it on a variety of inputs!

; <TODO: give your definition of interval->note and associated tests here>

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

; <TODO: give your definition of triad and associated teste here>

; <TODO: give the definitions of your three chosen chords and your one-word
; description of them here>