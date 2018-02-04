#lang racket
(require syntax/strip-context)

(require "racket-peg.rkt")
(require "peg-sequences.rkt")
(require "peg-in-peg.rkt")

(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax]))

(define (peg-port->scheme in)
  (peg->scheme (car
                (peg-result->object (peg (and peg (! (any-char))) (port->string in))))))

(define (literal-read in)
  (syntax->datum
   (literal-read-syntax #f in)))

(define (literal-read-syntax src in)
  (with-syntax ([body (peg-port->scheme in)])
    (strip-context
     #'(module anything racket
         (provide (all-defined-out))
         (require "racket-peg.rkt")
         body))))
