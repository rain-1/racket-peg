#lang info
(define version "0.3")
(define collection "peg")
(define deps '("base" "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/peg.scrbl" ())))
(define compile-omit-paths '("peg-src"))
(define pkg-desc "A PEG parser generator")
