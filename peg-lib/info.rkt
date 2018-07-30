#lang info

(define collection 'multi)

(define version "0.2")

(define deps
  '("base"
    "rackunit-lib"))

(define build-deps
  '("scribble-lib"
    "racket-doc"))

(define scribblings '(("scribblings/peg.scrbl" ())))

(define pkg-desc "A PEG parser generator")
