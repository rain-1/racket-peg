#lang racket

(require peg)
(require rackunit)

(require "../peg-syntax/peg-example-named-pattern.rkt")

;; As we have this named patterns, this don't explode, but don't change nothing
;; because we don't have semantic actions(yet)
(check-equal?
  (peg sum  "123+12")
  '(sum (number . "123") (number . "12")))
  
  
