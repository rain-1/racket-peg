#lang racket

(require peg)
(require rackunit)
(require "../peg-syntax/peg-example-comma-separated-number.rkt")

(check-equal? (peg file "123,12,10,21")
              '(file (number "123") (number "12") (number "10") (number "10") (number "21")))
