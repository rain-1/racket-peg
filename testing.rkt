#lang racket

(require rackunit)

(require "racket-peg.rkt")
(require "peg-sequences.rkt")

;;;;
;; testing it

(define-peg digit (or (char #\0) (call nonzero)))
(define-peg nonzero (range "123456789"))
(define-peg number (or (char #\0)
                       (name n (and (call nonzero) (* (call digit)))))
  (if n (string->number n) 0))
(define-peg pm-number (and (? (name neg (char #\-))) (name n (call number)))
  (if neg (- n) n))

(check-equal? (peg pm-number "0") 0)
(check-equal? (peg pm-number "01") 0)
(check-equal? (peg pm-number "321") 321)
(check-equal? (peg pm-number "-321") -321)
(check-equal? (peg pm-number "100") 100)

(define-peg symbol (and (name s (+ (and (! (char #\space))
                                        (! (char #\())
                                        (! (char #\)))
                                        (any-char))))
                        (* (char #\space)))
  (string->symbol s))
(define-peg symbols (name res (* symbol)) res)
(define-peg sexp (or (and (char #\()
                          (name res (* (call sexp)))
                          (char #\))
                          (* (char #\space)))
                     (name res (call symbol)))
  res)

(check-equal? (peg symbol "foo") 'foo)
(check-equal? (peg symbol "bar baz") 'bar)
(check-equal? (peg symbols "bar baz") '(bar baz))
(check-equal? (peg sexp "()") '())
(check-equal? (peg sexp "(a)") '(a))
(check-equal? (peg sexp "(a b)") '(a b))
(check-equal? (peg sexp "(a b c)") '(a b c))
(check-equal? (peg sexp "(foob (ar baz)quux)") '(foob (ar baz) quux))
(check-equal? (peg sexp "((())(()(())))") '((())(()(()))))
(check-equal? (peg sexp "(((o))(u(u)((e)x))o)") '(((o))(u(u)((e)x))o))
(check-equal? (peg sexp "(lambda (x) (list x (list (quote quote) x)))") '(lambda (x) (list x (list (quote quote) x))))

;(peg sexp "(")
;(peg sexp "(foo")
;(peg sexp "(((foo((((bar(((baz(((")
;(peg sexp "(((foo((((bar(((baz(((quux")

(define-peg +-minus
  (name res (or #\+ #\-))
  (case (string->symbol res)
    ((+) +)
    ((-) -)))
(define-peg expr-sum
  (and (name n1 expr-factor) (? (and (name op +-minus) (name n2 expr-sum))))
  (if n2 (op n1 n2) n1))
(define-peg expr-factor
  (and (name n1 expr-atom) (? (and #\* (name n2 expr-factor))))
  (if n2 (* n1 n2) n1))
(define-peg expr-atom
  (or (and #\( (name res expr-sum) #\))
      (name res pm-number))
  res)

(check-equal? (peg expr-sum "7*(2+3)") 35)
(check-equal? (peg expr-sum "7*2+3") 17)
(check-equal? (peg expr-sum "(7*2)+3") 17)


(define-peg regex-range
  (and #\[ (? (name neg #\^)) (name res (* (or regex-range-range regex-range-single))) #\])
  (if neg `(negate . ,res) res))
(define-peg regex-range-range
  (and (name c1 (any-char)) #\- (name c2 (any-char)))
  `(range ,c1 ,c2))
(define-peg regex-range-single
  (name c1 (and (! #\-) (any-char)))
  `(single ,c1))

(check-equal? (peg regex-range "[a-zA-Z0-9_]")
              '((range "a" "z") (range "A" "Z") (range "0" "9") (single "_")))
(check-equal? (peg regex-range "[^0-9]") '(negate (range "0" "9")))


;; https://www.gnu.org/software/guile/manual/html_node/PEG-Tutorial.html#PEG-Tutorial

(define *etc-passwd*
  "root:x:0:0:root:/root:/bin/bash
daemon:x:1:1:daemon:/usr/sbin:/bin/sh
bin:x:2:2:bin:/bin:/bin/sh
sys:x:3:3:sys:/dev:/bin/sh
nobody:x:65534:65534:nobody:/nonexistent:/bin/sh
messagebus:x:103:107::/var/run/dbus:/bin/false
")

(define-peg passwd (name res (* entry))
  res)
(define-peg/tag entry (and login c pass c uid c gid c name-or-comment c homedir c shell newline+))

(define-peg text (* (and (! #\newline) (! c) (any-char))))
(define-peg/drop c #\:)
(define-peg/drop newline+ (+ #\newline))

(define-peg/tag login text)
(define-peg/tag pass text)
(define-peg/tag uid (* digit))
(define-peg/tag gid (* digit))
(define-peg/tag name-or-comment text)
(define-peg/tag homedir path)
(define-peg/tag shell path)
(define-peg path (* (and (drop #\/) path-element)))
(define-peg path-element (name res (+ (and (! #\/) (! #\newline) (! c) (any-char))))
  res)

(check-equal? (peg passwd *etc-passwd*)
              '((entry
                 (login . "root")
                 (pass . "x")
                 (uid . "0")
                 (gid . "0")
                 (name-or-comment . "root")
                 (homedir "root")
                 (shell "bin" "bash"))
                (entry
                 (login . "daemon")
                 (pass . "x")
                 (uid . "1")
                 (gid . "1")
                 (name-or-comment . "daemon")
                 (homedir "usr" "sbin")
                 (shell "bin" "sh"))
                (entry
                 (login . "bin")
                 (pass . "x")
                 (uid . "2")
                 (gid . "2")
                 (name-or-comment . "bin")
                 (homedir "bin")
                 (shell "bin" "sh"))
                (entry
                 (login . "sys")
                 (pass . "x")
                 (uid . "3")
                 (gid . "3")
                 (name-or-comment . "sys")
                 (homedir "dev")
                 (shell "bin" "sh"))
                (entry
                 (login . "nobody")
                 (pass . "x")
                 (uid . "65534")
                 (gid . "65534")
                 (name-or-comment . "nobody")
                 (homedir "nonexistent")
                 (shell "bin" "sh"))
                (entry
                 (login . "messagebus")
                 (pass . "x")
                 (uid . "103")
                 (gid . "107")
                 (name-or-comment)
                 (homedir "var" "run" "dbus")
                 (shell "bin" "false"))))

(define-peg/tag cfunc (and cSP ctype cSP cname cSP cargs cLB cSP cbody cRB))
(define-peg/tag ctype cidentifier)
(define-peg/tag cname cidentifier)
(define-peg cargs (and cLP (* (and (! (and cSP cRP)) carg cSP (or cCOMMA cRP) cSP)) cSP)) ;; !
(define-peg carg (and cSP ctype cSP cname))
(define-peg/tag cbody (* cstatement))
(define-peg cidentifier (and (or (range "abcdefghijklmnopqrstuvwxyz")
                                 (range "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
                             (* (or (range "abcdefghijklmnopqrstuvwxyz")
                                    (range "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                                    (range "0123456789")
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
