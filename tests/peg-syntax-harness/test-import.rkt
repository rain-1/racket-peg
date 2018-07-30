#lang racket

(require rackunit)
(require peg)
(require "../peg-syntax/peg-example-import.rkt")

(check-equal?
  (peg presentation "this is a well formed expr 4324+431")
  '(presentation "this is a well formed expr " (sum (product value number . "4324") "+" (sum product value number . "431"))))
