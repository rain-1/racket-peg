#lang racket

(require rackunit)

(require "racket-peg.rkt")
(require "peg-sequences.rkt")

(provide peg-rule:peg
         peg->scheme)

(define-peg/drop sp (* (or #\space #\tab #\newline)))

; from http://git.savannah.gnu.org/cgit/guile.git/tree/module/ice-9/peg/string-peg.scm

;; ISSUES:
;; changed * to ? in the suffix rule since we dont need multiple suffixes
;; was splitting up atoms: pattern -> patter,n to fix this i changed nonterminal to use nt
;; was going really far with csingle [.....]..]...], to fix disallowed ] from csingle
;; Added ; at the end of ever rule (for speed? why?)

(define *peg-as-peg*
#<<EOF
grammar <-- (nonterminal ('<--' / '<-' / '<') sp pattern)+ ';' ;
pattern <-- alternative (SLASH sp alternative)* ;
alternative <-- ([!&]? sp suffix)+ ;
suffix <-- primary ([*+?] sp)? ;
primary <-- '(' sp pattern ')' sp / '.' sp / literal / charclass / nonterminal !'<' ;
literal <-- ['] (!['] .)* ['] sp ;
charclass <-- LB (!']' (CCrange / CCsingle))* RB sp ;
CCrange <-- . '-' . ;
CCsingle <-- !']' . ;
ntchar <- [a-zA-Z0-9-] ;
nonterminal <-- ntchar+ !ntchar sp ;
sp < [ \t\n]* ;
SLASH < '/' ;
LB < '[' ;
RB < ']' ;
EOF
  )

(define-peg/tag peg (and sp (+ peg-rule)))
(define-peg/tag peg-rule (and nonterminal (or "<--" "<-" "<") sp pattern ";" sp))
(define-peg/tag pattern (and alternative (* (drop "/") sp alternative)))
(define-peg/tag alternative (+ (? #\!) sp suffix))
;(define-peg suffix (and primary (* (or #\* #\+ #\?) sp)))
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

;; https://www.gnu.org/software/guile/manual/html_node/PEG-Tutorial.html

(define *guile-peg-tutorial-passwd*
  "passwd <-- entry* !. ;
entry <-- login C pass C uid C gid C nameORcomment C homedir C shell NL* ;
login <-- text ;
pass <-- text ;
uid <-- [0-9]* ;
gid <-- [0-9]* ;
nameORcomment <-- text ;
homedir <-- path ;
shell <-- path ;
path <-- (SLASH pathELEMENT)* ;
pathELEMENT <-- (!NL !C  !'/' .)* ;
text <- (!NL !C  .)* ;
C < ':' ;
NL < '\n' ;
SLASH < '/' ;
")

(define *guile-peg-tutorial-arith*
  "expr <- sum ;
sum <-- (product ('+' / '-') sum) / product ;
product <-- (value ('*' / '/') product) / value ;
value <-- number / '(' expr ')' ;
number <-- [0-9]+ ;")


(define *guile-peg-tutorial-cfunc*
  "cfunc <-- cSP ctype cSP cname cSP cargs cLB cSP cbody cRB ;
ctype <-- cidentifier ;
cname <-- cidentifier ;
cargs <-- cLP (! (cSP cRP) carg cSP (cCOMMA / cRP) cSP)* cSP ;
carg <-- cSP ctype cSP cname ;
cbody <-- cstatement * ;
cidentifier <- [a-zA-z][a-zA-Z0-9_]* ;
cstatement <-- (!';'.)*cSC cSP ;
cSC < ';' ;
cCOMMA < ',' ;
cLP < '(' ;
cRP < ')' ;
cLB < '{' ;
cRB < '}' ;
cSP < [ \t\n]* ;")

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

;primary <-- '(' sp pattern ')' sp / '.' sp / literal / charclass / nonterminal !'<' ;
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

;(pretty-print
; (peg->scheme (peg peg *guile-peg-tutorial-arith*)))
