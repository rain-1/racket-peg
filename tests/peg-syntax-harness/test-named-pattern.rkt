#lang racket

(require peg)
(require rackunit)

(require "../peg-syntax/peg-example-named-pattern.rkt")

(check-equal?
  (peg sum  "123+12")
  '(+ 123 12))
