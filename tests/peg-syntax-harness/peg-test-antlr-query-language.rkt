#lang racket

(require rackunit)
(require peg)

(require "../peg-syntax/peg-example-antlr-query-language.rkt")

(check-equal? (peg expression "location within 10 km from (-37.814, 144.963) and status.stateOfCharge < 10%")
              '(predicate "location within " (amount (float . "10") " km ") "from " (location (float . "-37.814") (float . "144.963"))))
