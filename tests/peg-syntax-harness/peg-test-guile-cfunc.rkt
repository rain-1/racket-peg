(use-modules (racket-peg peg))
(use-modules (racket-peg guile-heredoc))
(use-modules (racket-peg rackunit))

#<<PEG

cfunc <-- cSP ctype cSP cname cSP cargs cLB cSP cbody cRB ;
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
cSP < [ \t\n]* ;

PEG

(check-equal? (peg cfunc "int square(int a) { return a*a;}")
              '(cfunc (ctype . "int") (cname . "square") (cargs (carg (ctype . "int") (cname . "a")))
                      (cbody (cstatement . "return a*a"))))
(check-equal? (peg cfunc "int mod(int a, int b) { int c = a/b;return a-b*c; }")
              '(cfunc (ctype . "int") (cname . "mod") (cargs (carg (ctype . "int") (cname . "a")) (carg (ctype . "int") (cname . "b")))
                      (cbody (cstatement . "int c = a/b")
                             (cstatement . "return a-b*c"))))
