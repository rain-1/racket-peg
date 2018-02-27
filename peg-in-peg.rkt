#lang peg

nt-char <- [a-zA-Z0-9\-] ;
nonterminal <-- nt-char+ !nt-char SP ;
SP < [ \t\n]* ;

literal <-- SQ (BS ['\\] / !['\\] .)* SQ SP ;
SQ < ['] ;
BS < [\\] ;

charclass <-- LB '^'? (cc-single / cc-escape / cc-range)+ RB SP ;
cc-single <-- cc-char ;
cc-escape <-- BS . ;
cc-range <-- cc-char DASH cc-char ;
cc-char <- !cc-escape-char . ;
cc-escape-char <- '[' / ']' / '-' / '^' / '\\' / 'n' / 't' ;
LB < '[' ;
RB < ']' ;
DASH < '-' ;

peg <-- SP grammar+ ;
grammar <-- (nonterminal ('<--' / '<-' / '<') SP pattern) ';' SP ;
pattern <-- alternative (SLASH SP alternative)* ;
alternative <-- expression+ ;
expression <-- [!]? SP primary ([*+?] SP)? ;
primary <-- '(' SP pattern ')' SP / '.' SP / literal / charclass / nonterminal ;
SLASH < '/' ;

