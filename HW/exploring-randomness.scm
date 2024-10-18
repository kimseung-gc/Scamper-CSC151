;; CSC 151-03 (Fall 2022)
;; Lab: Exploring Randomness
;; Authors: Joyce Gill, Seunghyeon Kim
;; Date: THE DATE HERE
;; Acknowledgements:
;;   ACKNOWLEDGEMENTS HERE

(import music)

;; -----------
"Introduction"
;; -----------

;; In this lab, we will explore exactly one new function in the standard
;; library: random! Try executing this program now to observe how calls
;; to random work.

(random 10)

(random 10)

(random 10)

;; You should see that (random 10) evaluates to a number in the range 0
;; to 10 (exclusive, i.e., 0--9). Try running the program a few times
;; and observe how on each invocation of the program, the result of
;; random is just that, random!
;;
;; Up until this point, our programs have always behaved deterministically,
;; that is, they have always produced the same output, no matter how many times
;; we executed them. While convenient, sometimes we want to have our programs
;; exhibit "random" behavior. The random function in Scheme allows us to do
;; just that!

;; -----------------------
"Problem 1: Guessing Game"
;; -----------------------

;; Using random, write a function (guessing-game n guess) that takes two inputs:
;;
;; + n, a non-negative integer, is a bound on the guessing game.
;; + guess, a non-negative integer, is a number that you, the user, guesses.
;;
;; The function generates a random number in the range 0 to n-1. If guess is
;; equal to that number, then the function outputs "you win!" Otherwise, the
;; function outputs "you lose!"
;;
;; (guessing-game 10 3)  [internally, the function randomly generates 3]
;; > "you win!"
;; (guessing-game 10 3)  [internally, the function randomly generates 8]
;; > "you lose!"

(define guessing-game
  (lambda (n guess)
    (equal? (random n) guess)
  )
)

;; -----------------------
"Problem 2: Random choice"
;; -----------------------

;; Using random, it is convenient to write additional functions to perform
;; random behavior over other data types. Write a function (choose lst) that
;; returns a random element of lst.
;;
;; (choose (list "a" "b" "c"))
;; > "a"
;; (choose (list "a" "b" "c"))
;; > "a"
;; (choose (list "a" "b" "c"))
;; > "c"
;; (choose (list "a" "b" "c"))
;; > "b"
;; [Results are random!]
;;
;; To implement this function, recall that the Prelude functions length
;; returns the length of a list and list-ref returns the element at a specific
;; index of the list.

(define choose
  ;; TODO: implement me!
  "...")

;; -------------------
"Problem 3: Being Eno"
;; -------------------

;; Reverb Machine recently released an excellent article about the music of
;; Brian Eno, a pioneer in ambient and electronic music. Oddly enough, Brian
;; Eno was not a trained musician---he couldn't read or write traditional music!
;; Nevertheless, electronics gave him the capability to infuse the world of
;; of music with his unique artistic vision.
;;
;; https://reverbmachine.com/blog/deconstructing-brian-eno-music-for-airports/
;; https://en.wikipedia.org/wiki/Brian_Eno
;;
;; Brian Eno's Ambient 1: Music for Airports featured musical tracks composed
;; of a small number of musical loops. In this problem, we'll use randomness
;; to stitch together these loops in novel ways, ala, the Reverb Machine
;; article linked above!
;;
;; Below are implementations of the various loops Eno used for the track
;; entitled "1/2":

(define loop-1
  (seq
    (note 72 wn)        ; 1 sec
    (rest (dur 16 1))   ; 16 secs
    (rest (dur 1 10)))) ; 0.1 secs

"loop-1"
loop-1

(define loop-2
  (seq
    (note 81 hn)        ; 0.5 secs
    (note 77 hn)        ; 0.5 secs
    (note 65 wn)        ; 1 sec
    (rest (dur 21 1))   ; 21 secs
    (rest (dur 2 10)))) ; 0.2 secs

"loop-2"
loop-2

(define loop-3
  (seq 
    (note 65 wn)        ; 1 sec
    (note 60 wn)        ; 1 sec
    (note 63 wn)        ; 1 sec
    (note 60 wn)        ; 1 sec
    (rest (dur 25 1))   ; 25 sec
    (rest (dur 1 2))))  ; 0.5 secs

"loop-3"
loop-3

(define loop-4
  (seq
    (note 48 wn)         ; 1 sec
    (rest (dur 30 1))    ; 30 secs
    (rest (dur 1 10))))  ; 0.1 secs
  
"loop-4"
loop-4

(define loop-5
  (seq
    (note 57 qn)         ; 0.25 secs
    (note 65 (dur 3 4))  ; 0.75 secs
    (note 63 wn)         ; 1 sec
    (note 60 wn)         ; 1 sec
    (rest (dur 16 1))    ; 16 sec
    (rest (dur 8 10))))  ; 0.8 secs

"loop-5"
loop-5

(define loop-6
  (seq
    (note 65 sn)
    (note 72 sn)
    (note 77 sn)
    (note 79 sn)
    (rest (dur 3 4))     ; 1 sec
    (rest (dur 27 1))))  ; 27 sec

"loop-6"
loop-6

(define loop-7
  (seq
    (par
      (seq (note 63 (dur 3 4))
           (note 60 wn))
      (note 48 wn))       ; 1.75 sec
    (rest qn)             ; 0.25 sec
    (rest (dur 28 1))))   ; 28 secs

"loop-7"
loop-7
        
(define loop-8
  (seq
    (note 51 (dur 2 1))  ; 2 secs
    (note 44 (dur 2 1))  ; 2 secs
    (rest (dur 36 1))))  ; 36 secs

"loop-8"
loop-8

;; Eno composed "1/2" by playing these eight loops repeating in parallel.
;; Recreate "1/2" using `par` and `repeat` to create a function `(eno n)`
;; that plays all eight loops in parallel. Each loop repeats n times.

(define eno
  ;; TODO: implement me!
  (note 60 qn))

;; Because of the delay in the loops, the regular version of Eno is
;; quite slow! To "test" your function, you can use `mod` in conjunction
;; with `tempo` to speed things up. The default tempo is 120 beats per
;; minute with the beat being a quarter note. To double the tempo, you
;; could use write:
;;
;; (mod (tempo qn 240) (eno 20))

;; ----------------------
"Problem 4: Randomly Eno"
;; ----------------------

;; In listening to `(eno n)` note that the "music" feels random because the
;; loops are not all the same lengths. This creates irregular cycles of
;; musical patterns that "feel" random and etheral as a result. However
;; note that the patterns aren't actually random as they don't invoke our
;; random number generator. This composition is an example of a general
;;
;; One way to experiment musically with this composition is to allow the
;; durations of each loop to be randomly determined rather than fixed!
;; In the code for loops, I've annotated how each loop obtains its
;; prescribed duration according to the Reverb Machine article. To conclude
;; this lab, go into each loop's definition and make the duration of the loop
;; random by replacing the appropriate hard-coded values with calls to random.
;; For example, in loop-1, there is the following function call:
;;
;;   (rest (dur 16 1))   ; 16 secs
;;
;; This creates a musical rest or pause for 16/1 = 16 units of time. By
;; default, 1 unit of time is 1 second, so this results in a rest of 16
;; seconds. Replacing 16 with an appropriate call to random will make the
;; duration of this rest random instead of fixed!
;;
;; Experiement with different random durations and your (eno n) function and
;; see if you can come up with any sonically interesting compositions! 

(define RanDur
  (lambda (n)
    (dur (random n) (random n))
  )
)

(define eno2
  (lambda (n1 n2)
    (note)
  )
)