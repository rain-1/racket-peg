#lang peg
peg-in-peg <-- sp grammar+ ;
grammar <-- (nonterminal ('<--' / '<-' / '<') sp pattern)+ ';' sp ;
pattern <-- alternative (SLASH sp alternative)* ;
alternative <-- ([!&]? sp suffix)+ ;
suffix <-- primary ([*+?] sp)? ;
primary <-- '(' sp pattern ')' sp / '.' sp / literal / charclass / nonterminal !'<' ;
literal <-- ['] (!['] .)* ['] sp ;
charclass <-- LB (!']' (CCrange / CCsingle))* RB sp ;
CCrange <-- . '-' . ;
CCsingle <-- !']' . ;
ntchar <- [a-zA-Z0-9\-] ;
nonterminal <-- ntchar+ !ntchar sp ;
sp < [ \t\n]* ;
SLASH < '/' ;
LB < '[' ;
RB < ']' ;

