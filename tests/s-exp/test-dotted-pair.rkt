#lang racket

(require rackunit)
(require peg)
(require peg/s-exp)

(check-equal?
 (peg s-exp "()")
 '())

(check-equal?
 (peg s-exp "(a . b)")
 '(a . b))

(check-equal?
 (peg s-exp "(a)")
 '(a))

(check-equal?
 (peg s-exp "(a b)")
 '(a b))

(check-equal?
 (peg s-exp "(a b . c)")
 '(a b . c))

(check-equal?
 (peg s-exp "(a b c . d)")
 '(a b c . d))

(check-equal?
 (peg s-exp "(a b c d . e)")
 '(a b c d . e))
