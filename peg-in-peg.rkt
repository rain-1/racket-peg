#lang peg

nt-char <- [a-zA-Z0-9_\-] ;
nonterminal <-- nt-char (nt-char / [./])* !nt-char SP ;
SP < (comment / [ \t\n])* ;
comment < '//' [^\n]* ;

literal <-- SQ (BS ['\\] / !['\\] .)* SQ SP ;
SQ < ['] ;
BS < [\\] ;

charclass <-- LB '^'? (cc-range / cc-escape / cc-single)+ RB SP ;
cc-range <-- cc-char DASH cc-char ;
cc-escape <-- BS . ;
cc-single <-- cc-char ;
cc-char <- !cc-escape-char . / 'n' / 't' ;
cc-escape-char <- '[' / ']' / '-' / '^' / '\\' / 'n' / 't' ;
LB < '[' ;
RB < ']' ;
DASH < '-' ;

peg <-- SP import* grammar+ ;
import <-- 'import' SP nonterminal ';' SP ;
grammar <-- (nonterminal ('<--' / '<-' / '<') SP pattern) ';' SP ;
pattern <-- alternative (SLASH SP alternative)* ;
alternative <-- expression+ ;
expression <-- [!&]? SP primary ([*+?] SP)? ;
primary <-- '(' SP pattern ')' SP / '.' SP / literal / charclass / nonterminal ;
SLASH < '/' ;
