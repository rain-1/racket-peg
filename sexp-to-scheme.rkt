#lang racket

(provide s-exp->scheme)

(define (s-exp->scheme s-exp)
  (match s-exp
    (`(list . ,xs) (map s-exp->scheme xs))
    
    (`(boolean . "#t") #t)
    (`(boolean . "#f") #f)

    (`(identifier . ,s) (string->symbol s))

    (`(number . ,n) (string->number n))

    (`(string . ,s) s)

    ((list 'quote xs) (list 'quote (s-exp->scheme xs)))
    
    ((list 'quasiquote xs) (list 'quasiquote (s-exp->scheme xs)))
    
    ((list 'unquote xs) (list 'unquote (s-exp->scheme xs)))
    
    (else (error 's-exp->scheme "not an s-expression AST ~a" s-exp))))
