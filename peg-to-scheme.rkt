#lang racket

(require rackunit)
(require racket/trace)

(require peg/peg)
(require peg/peg-result)
(require peg/peg-in-peg-expanded)

(provide peg->scheme)

(define (make-and lst)
  (cond ((null? lst) (error 'make-and "null"))
        ((null? (cdr lst)) (car lst))
        (else `(and . ,lst))))

(define (make-or lst)
  (cond ((null? lst) (error 'make-or "null"))
        ((null? (cdr lst)) (car lst))
        (else `(or . ,lst))))

(define (string->char str)
  (unless (and (string? str) (= 1 (string-length str)))
    (error 'string->char "~a" str))
  (string-ref str 0))

(define (semantic? l)
  (and (pair? l)
       (equal? (car l) 'semantic)))

(define (grammar? l)
  (and (pair? l)
       (equal? (car l) 'grammar)))


(define (peg->scheme p)
  (match p
    (`(peg ,(? semantic? a) ... ,(? grammar? b) ...)
     `(begin ,@(map peg->acheme:semantic a) ,@(map peg->scheme:grammar b)))
    (else (error 'peg->scheme "~s" p))))

(define (peg->scheme:semantic a)
  (match a
    (`(semantic ,literal) `(require ,literal))))


(define (peg->scheme:grammar p)
  (define (op? op)
    (case op
      (("<") 'define-peg/drop)
      (("<-") 'define-peg)
      (("<--") 'define-peg/tag)
      (else (error 'peg->scheme:grammar "~s" op))))
  (match p
    (`(grammar (nonterminal . ,nt) ,op ,pat ";")
     (let ((op^ (op? op)))
       `(,op^ ,(string->symbol nt) ,(peg->scheme:pattern pat))))
    (else (error 'peg->scheme:grammar "~s" p))))

(define (peg->scheme:pattern p)
  (match p
    (`(pattern . ,alternatives)
     (make-or (map peg->scheme:alternative alternatives)))
    (else (error 'peg->scheme:pattern "~s" p))))

(define (peg->scheme:alternative p)
  (match p
    (`(alternative . ,exps)
     (make-and (map peg->scheme:expression exps)))
    (else (error 'peg->scheme:alternative "~s" p))))

(define (peg->scheme:expression p)
  (define (prefix-op? extra)
    (case extra
      (("!") '!)
      (("&") '&)
      (else (error 'peg->scheme:expression "invalid prefix op" extra))))
  (define (op? extra)
    (case extra
      (("*") '*)
      (("+") '+)
      (("?") '?)
      (else (error 'peg->scheme:expression "invalid op" extra))))
  (define (go negate? prim extra?)
    ((lambda (x) (if negate? `(,negate? ,x) x))
     ((lambda (x) (if extra? `(,extra? ,x) x))
      prim)))
  (match p
    (`(expression ,prim)
     (go #f (peg->scheme:primary prim) #f))
    (`(expression ,p-op ,prim) #:when (string? p-op)
     (go (prefix-op? p-op) (peg->scheme:primary prim) #f))
    (`(expression ,prim ,extra)
     (go #f (peg->scheme:primary prim) (op? extra)))
    (`(expression ,p-op ,prim ,extra) #:when (string? p-op)
     (go (prefix-op? p-op) (peg->scheme:primary prim) (op? extra)))
    (else (error 'peg->scheme:expression "~s" p))))

(define (peg->scheme:primary p)
  (match p
    (`(primary "(" ,pat ")")
     (peg->scheme:pattern pat))
    (`(primary . ".")
     `(any-char))
    (`(primary literal . ,lit)
     lit)
    (`(primary charclass "^" . ,cc)
     `(and (! ,(make-or (map peg->scheme:cc cc))) (any-char)))
    (`(primary charclass . ,cc)
     (make-or (map peg->scheme:cc cc)))
    (`(primary nonterminal . ,nt)
     (string->symbol nt))
    (else (error 'peg->scheme:primary "~s" p))))

(define (peg->scheme:cc p)
  (define (unescape? ch)
    (case ch
      ((#\[) #\[)
      ((#\]) #\])
      ((#\-) #\-)
      ((#\^) #\^)
      ((#\\) #\\)
      ((#\n) #\newline)
      ((#\t) #\tab)
      (else (error 'peg->scheme:ccrange "escaping a character that need not be escaped: ~s" ch))))
  (define (length-2? str) (and (string? str) (= 2 (string-length str))))
  (match p
    (`(cc-single . ,ch)
     (string->char ch))
    (`(cc-escape . ,ch)
     (unescape? (string->char ch)))
    (`(cc-range . ,c1c2)
     (unless (length-2? c1c2)
       (error 'peg->scheme:cc "invalid char class range ~s" c1c2))
     `(range ,(string-ref c1c2 0) ,(string-ref c1c2 1)))
    (else (error 'peg->scheme:cc "~s" p))))

;(trace peg->scheme)
;(trace peg->scheme:grammar)
;(trace peg->scheme:pattern)
;(trace peg->scheme:alternative)
;(trace peg->scheme:expression)
;(trace peg->scheme:primary)
