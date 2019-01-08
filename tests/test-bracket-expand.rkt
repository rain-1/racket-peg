#lang racket

(require rackunit)
(require peg)
(require "peg-syntax/peg-example-bracket-expand.rkt")

(check-equal? (peg items "a{x{0,1},w}b")
              '("ax0b" "ax1b" "awb"))


