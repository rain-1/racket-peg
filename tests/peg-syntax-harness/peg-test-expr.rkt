#lang racket

(require rackunit)
(require peg)

(require "../peg/peg-example-expr.rkt")

(check-equal? (peg expr "4324+431")
              '(sum (product value number . "4324") "+" (sum product value number . "431")))
(check-equal? (peg expr "72*4324+431")
	      '(sum (product (value number . "72") "*" (product value number . "4324"))
                    "+" (sum product value number . "431")))
(check-equal? (peg expr "4324+72*431")
	      '(sum (product value number . "4324") "+"
                    (sum product (value number . "72") "*" (product value number . "431"))))
(check-equal? (peg expr "4324+72*431*27")
	      '(sum (product value number . "4324") "+"
                    (sum product (value number . "72") "*"
                         (product (value number . "431") "*" (product value number . "27")))))

