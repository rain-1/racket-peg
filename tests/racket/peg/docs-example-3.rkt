#lang racket

(require rackunit)
(require peg)

(define-peg _ (* (or #\space #\newline)))

(define-peg symbol
  (and (name res (+ (and (! #\( #\) #\space #\newline) (any-char)))) _)
  (string->symbol res))

(define-peg/bake sexp
  (or symbol
      (and (drop #\() (* sexp) (drop #\) _))))

(check-equal? (peg sexp "()") '())
(check-equal? (peg sexp "(a)") '(a))
(check-equal? (peg sexp "(a b)") '(a b))
(check-equal? (peg sexp "(a b c)") '(a b c))
(check-equal? (peg sexp "(foob (ar baz)quux)") '(foob (ar baz) quux))
(check-equal? (peg sexp "((())(()(())))") '((())(()(()))))
(check-equal? (peg sexp "(((o))(u(u)((e)x))o)") '(((o))(u(u)((e)x))o))
(check-equal? (peg sexp "(lambda (x) (list x (list (quote quote) x)))") '(lambda (x) (list x (list (quote quote) x))))
