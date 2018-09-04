#lang racket

(require rackunit)
(require racket/trace)

(require "peg.rkt")
(require "peg-result.rkt")
(require "peg-in-peg.rkt")

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

(define (peg->scheme p)
  (match p
    (`((first (name ,identifier)) (peg . ,grammars))
     `(begin
	(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax]))
	(define (literal-read in)
	  (syntax->datum
	   (literal-read-syntax #f in)))

	(define (literal-read-syntax src in)
	  (with-syntax ([body (peg (and ,(string->symbol identifier) (! (any-char))) (port->string in))])
	    (strip-context
	     #'(module anything racket
	         (provide (all-defined-out))
	         (require peg/peg)
	         body))))
     	. ,(map peg->scheme:grammar grammars)))
    (`(peg . ,grammars)
     `(begin . ,(map peg->scheme:grammar grammars)))
    (else (error 'peg->scheme "~s" p))))

(define (peg->scheme:grammar p)
  (define (op? op)
    (case op
      (("<") 'define-peg/drop)
      (("<-") 'define-peg)
      (("<--") 'define-peg/tag)
      (else (error 'peg->scheme:grammar "~s" op))))
  (match p
    (`(import ,s-exp ";")
     `,s-exp)
    (`(rule (name . ,nt) ,op ,pat ";")
     (let ((op^ (op? op)))
       `(,op^ ,(string->symbol nt) ,(peg->scheme:pattern pat))))
    (`(rule (name . ,nt) "<-" ,pat "->" ,sem ";")
     `(define-peg ,(string->symbol nt) ,(peg->scheme:pattern pat) ,sem))
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
      (("~") 'drop)
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
    (`(expression (name . ,n) . ,rest)
      `(name ,(string->symbol n) ,(peg->scheme:expression `(expression . ,rest))))
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
    (`(primary literal)
     "")
    (`(primary literal . ,lit)
     lit)
    (`(primary charclass "^" . ,cc)
     `(and (! ,(make-or (map peg->scheme:cc cc))) (any-char)))
    (`(primary charclass . ,cc)
     (make-or (map peg->scheme:cc cc)))
    (`(primary name . ,nt)
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
