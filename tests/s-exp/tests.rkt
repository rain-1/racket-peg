#lang racket

(require rackunit)
(require peg)
(require peg/s-exp)

(define (s-exp->scheme x) x)

(check-equal?
 (s-exp->scheme (peg s-exp "(\"foo bar\" baz (#t 244))"))
 '("foo bar" baz (#t 244)))

;; spacing test
(check-equal?
 (s-exp->scheme (peg s-exp "(a b c)"))
 '(a b c))
(check-equal?
 (s-exp->scheme (peg s-exp "( a b c)"))
 '(a b c))
(check-equal?
 (s-exp->scheme (peg s-exp "(a b c )"))
 '(a b c))


;; quote, unquote and quasiquote test
(check-equal?
 (s-exp->scheme (peg s-exp "'(a b c)"))
 '(quote (a b c)))
(check-equal?
 (s-exp->scheme (peg s-exp "`(a b ,(c d e))"))
 '(quasiquote (a b (unquote (c d e)))))
(check-equal?
 (s-exp->scheme (peg s-exp "'''x"))
 ''''x)
(check-equal?
 (s-exp->scheme (peg s-exp "'``'x"))
 ''``'x)
(check-equal?
 (s-exp->scheme (peg s-exp "'``'`'x"))
 ''``'`'x)

;; syntax quotes
(check-equal?
  (peg s-exp "#`(#,foo #,(list #'bar bar))")
  '#`(#,foo #,(list #'bar bar)))

;; + and numbers
(check-equal?
 (s-exp->scheme (peg s-exp "(+ 1 2)"))
 '(+ 1 2))

;; lambda expressions
(check-equal?
 (s-exp->scheme (peg s-exp "((lambda (x) (+ x 1)) 13)"))
 '((lambda (x) (+ x 1)) 13))

;; escaping inside strings
(check-equal?
 (s-exp->scheme (peg s-exp "(\"ABC\" \"A\\\\B\\\\C\" \"A\\\"B\\\"C\")"))
 '("ABC" "A\\B\\C" "A\"B\"C"))

(check-equal?
  (s-exp->scheme (peg s-exp "(string->number bar)"))
  '(string->number bar))

;; box lists
(check-equal?
 (s-exp->scheme (peg s-exp
		     "(define (f l)
                         (match
                          [(list a b) a]
                          [(list a) (list a 1)]))"))
	       '(define (f l)
		  (match
		   [(list a b) a]
		   [(list a) (list a 1)])))

(check-equal?
 (s-exp->scheme (peg s-exp
 "(struct add (a b) #:transparent)"))
 '(struct add (a b) #:transparent))

(check-equal?
 (s-exp->scheme (peg s-exp
 "(define (my-sort lst #:comparator [cmp <])
             (sort lst cmp))"))
 '(define (my-sort lst #:comparator [cmp <])
    (sort lst cmp)))

;; newline and tab escapes
(check-equal?
 (peg s-exp "\"abc\\ndef\"")
 "abc\ndef")
(check-equal?
 (peg s-exp "\"abc\\tdef\"")
 "abc\tdef")
