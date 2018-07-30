#lang racket

(require rackunit)
(require peg)

(require "../peg-syntax/peg-example-guile-cfunc.rkt")

(check-equal? (peg cfunc "int square(int a) { return a*a;}")
              '(cfunc (ctype . "int") (cname . "square") (cargs (carg (ctype . "int") (cname . "a")))
                      (cbody (cstatement . "return a*a"))))
(check-equal? (peg cfunc "int mod(int a, int b) { int c = a/b;return a-b*c; }")
              '(cfunc (ctype . "int") (cname . "mod") (cargs (carg (ctype . "int") (cname . "a")) (carg (ctype . "int") (cname . "b")))
                      (cbody (cstatement . "int c = a/b")
                             (cstatement . "return a-b*c"))))
