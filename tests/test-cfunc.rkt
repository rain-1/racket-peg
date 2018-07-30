#lang racket

(require rackunit)
(require peg)

;; https://www.gnu.org/software/guile/manual/html_node/PEG-Tutorial.html#PEG-Tutorial

(define-peg/tag cfunc (and cSP ctype cSP cname cSP cargs cLB cSP cbody cRB))
(define-peg/tag ctype cidentifier)
(define-peg/tag cname cidentifier)
(define-peg cargs (and cLP (* (and (! (and cSP cRP)) carg cSP (or cCOMMA cRP) cSP)) cSP)) ;; !
(define-peg carg (and cSP ctype cSP cname))
(define-peg/tag cbody (* cstatement))
(define-peg cidentifier (and (or (range #\a #\z)
                                 (range #\A #\Z))
                             (* (or (range #\a #\z)
                                    (range #\A #\Z)
                                    (range #\0 #\9)
                                    #\-))))
(define-peg cstatement (name res (and (* (and (! #\;) (any-char))) cSC cSP))
  res)
(define-peg/drop cSC #\;)
(define-peg/drop cCOMMA #\,)
(define-peg/drop cLP #\()
(define-peg/drop cRP #\))
(define-peg/drop cLB #\{)
(define-peg/drop cRB #\})
(define-peg/drop cSP (* (or #\space #\newline)))

(check-equal? (peg cfunc "int square(int a) { return a*a;}")
              '(cfunc (ctype . "int") (cname . "square") (ctype . "int") (cname . "a") (cbody "return a*a")))
(check-equal? (peg cfunc "int mod(int a, int b) { int c = a/b;return a-b*c; }")
              '(cfunc (ctype . "int") (cname . "mod") (ctype . "int") (cname . "a") (ctype . "int") (cname . "b") (cbody "int c = a/b" "return a-b*c")))

