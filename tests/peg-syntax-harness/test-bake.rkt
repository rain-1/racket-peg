#lang racket

(require rackunit)
(require peg)
(require "../peg-syntax/peg-example-bake.rkt")

(check-equal?
	(peg number "123")
	"123")

(check-equal?
	(peg sum "12+45")
	'("12" "+" "45"))
