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
