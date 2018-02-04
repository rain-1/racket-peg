#lang racket

(require rackunit)

(require peg/peg)
(require peg/peg-result)

(provide peg-rule:peg
         peg->scheme)

;; The peg definition here is based on translating the guile string-peg into our s-exp peg language.
;; http://git.savannah.gnu.org/cgit/guile.git/tree/module/ice-9/peg/string-peg.scm
;;
;; Modifications:
;; - changed * to ? in the suffix rule since we dont need multiple suffixes
;; - was splitting up atoms: pattern -> patter,n to fix this i changed nonterminal to use nt
;; - was going really far with csingle [.....]..]...], to fix disallowed ] from csingle
;; - Added ; at the end of ever rule (for speed? why?)

(define-peg/drop sp (* (or #\space #\tab #\newline)))

(define-peg/tag peg (and sp (+ peg-rule)))
(define-peg/tag peg-rule (and nonterminal (or "<--" "<-" "<") sp pattern ";" sp))
(define-peg/tag pattern (and alternative (* (drop "/") sp alternative)))
(define-peg/tag alternative (+ (? #\!) sp suffix))
(define-peg/tag suffix (and primary (? (or #\* #\+ #\?)) sp))
(define-peg/tag primary (or (and "(" sp pattern ")" sp)
                            (and "." sp)
                            literal
                            charclass
                            (and nonterminal (! "<"))))
(define-peg/tag literal (and (drop "'") (* (and (! "'") (any-char))) (drop "'") sp))
(define-peg/tag charclass (and (drop "[") (* (or ccrange ccsingle)) (drop "]") sp))
(define-peg ccrange (and (name c1 (any-char)) "-" (name c2 (any-char)))
  `(ccrange ,c1 ,c2))
(define-peg/tag ccsingle (and (! #\]) (any-char)))
(define-peg nt-char (or (range #\a #\z)
                        (range #\A #\Z)
                        (range #\0 #\9)
                        #\-))
(define-peg nonterminal (name res (and (+ nt-char) (! nt-char) sp))
  (string->symbol res))

;;; Translation
;; This translates the resulting ASTs we get from the peg parser
;; into actual scheme code making use of our define-peg macros

(define (peg->scheme p)
  (match p
    (`(peg . ,rules)
     `(begin . ,(map peg->scheme:peg-rule rules)))
    (else (error 'peg->scheme "~a" p))))

(define (peg->scheme:peg-rule r)
  (match r
    (`(peg-rule ,nm ,op ,pat ";")
     (let ((def (case op
                  (("<") 'define-peg/drop)
                  (("<-") 'define-peg)
                  (("<--") 'define-peg/tag)
                  (else (error 'peg->scheme:peg-rule "~a" op)))))
       `(,def ,nm ,(peg->scheme:pattern pat))))))

(define (peg->scheme:pattern r)
  (match r
    (`(pattern ,alt)
     (peg->scheme:alternative alt))
    (`(pattern . ,alts)
     `(or . ,(map peg->scheme:alternative alts)))
    (else (error 'peg->scheme:pattern "~a" r))))

(define (make-and lst)
  (cond ((null? lst)
         (error 'make-and "null"))
        ((null? (cdr lst))
         (car lst))
        (else
         `(and . ,lst))))

(define (peg->scheme:alternative a)
  (match a
    (`(alternative . ,things)
     (make-and (let loop ((things things))
                 (cond ((null? things) '())
                       ((equal? "!" (car things))
                        (if (null? (cdr things))
                            (error 'peg->scheme:alternative "AST ended with a negation")
                            (cons `(! ,(peg->scheme:suffix (cadr things)))
                                  (loop (cddr things)))))
                       (else (cons (peg->scheme:suffix (car things))
                                   (loop (cdr things))))))))
    (else 'peg->scheme:alternative "~a" a)))

(define (peg->scheme:suffix suf)
  (match suf
    (`(suffix ,p)
     (peg->scheme:primary p))
    (`(suffix ,p ,s)
     (let ((op (case s
                 (("*") '*)
                 (("+") '+)
                 (("?") '?)
                 (else (error 'peg->scheme:suffix "invalid op" s)))))
       `(,op ,(peg->scheme:primary p))))
    (else (error 'peg->scheme:suffix "~a" suf))))

(define (peg->scheme:primary pr)
  (match pr
    (`(primary "(" ,pat ")")
     (peg->scheme:pattern pat))
    (`(primary ".")
     '(any-char))
    (`(primary literal . ,str)
     str)
    (`(primary charclass . ,cc)
     `(or . ,(map peg->scheme:ccrange cc)))
    (`(primary ,nonterm)
     `(call ,nonterm))
    (else
     (error 'peg->scheme:primary "~a" pr))))

(define (string->char str)
  (unless (and (string? str) (= 1 (string-length str)))
    (error 'string->char "~a" str))
  (string-ref str 0))

(define (peg->scheme:ccrange cc)
  (match cc
    (`(ccrange ,c1 ,c2) `(range ,(string->char c1) ,(string->char c2)))
    (`(ccsingle ,c1) (string->char c1))
    (else (error 'peg->scheme:ccrange "~a" cc))))
