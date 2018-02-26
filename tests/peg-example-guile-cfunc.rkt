#lang peg
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
