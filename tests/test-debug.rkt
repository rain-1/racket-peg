#lang racket

(require rackunit)
(require peg)

(define-peg number (name v (+ (range #\0 #\9))) (string->number v))

(define-peg exp (or (and number (drop "+") exp) number))

(peg exp "12" #t)

(peg exp "12+13" #t)
