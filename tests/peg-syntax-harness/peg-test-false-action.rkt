
#lang racket

(require rackunit)
(require peg)

(require "../peg-syntax/peg-example-false-action.rkt")

(check-equal? (peg a "a") #f)

