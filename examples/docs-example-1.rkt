#lang racket

(require peg/peg)
(require rackunit)

(define *sentence* "the quick brown fox jumps over the lazy dog")

(define-peg non-space
  (and (! #\space) (any-char)))

(define-peg/bake word
  (and (+ non-space)
       (drop (? #\space))))

(check-equal? (peg (+ word) *sentence*)
              '("the" "quick" "brown" "fox" "jumps" "over" "the" "lazy" "dog"))

