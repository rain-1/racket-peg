#lang racket
(require peg-lib/peg)
(provide (all-from-out peg-lib/peg))
(module reader racket

(require syntax/strip-context)

(require peg-lib/peg)
(require peg-lib/peg-result)
(require peg-lib/peg-in-peg-expanded)
(require peg-lib/peg-to-scheme)

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
         (require peg/peg)
         body))))

)
