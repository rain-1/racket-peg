#lang racket

(require rackunit)
(require peg)
(require peg/sexp-parser-expanded)

(check-equal?
  (peg s-exp "10e4")
  10e4)
  
(check-equal?
  (peg s-exp "(1 2 10.5e2)")
  '(1 2 10.5e2))
  
(check-equal?
  (peg s-exp "(1 2 -10.4e2 0 #\\z)")
  '(1 2 -10.4e2 0 #\z))
