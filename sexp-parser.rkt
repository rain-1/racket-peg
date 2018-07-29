#lang peg

_ < [ \t\n]*;
OB < '(' ;
CB < ')' ;
DQ < ["] ;
BS < [\\] ;

s-exp <-  list / quote / quasiquote / unquote  / atom;

atom <- boolean / number / s-identifier / string ;
list <-- OB _ (s-exp _)*  CB ;

boolean <-- '#t' / '#f' ;
s-identifier <--   [^ ()\[\]{}",'`;#|\\]+ ;
number <-- [0-9]+ ;
string <-- DQ ([^"\\] / BS .)* DQ ;

SQ < '\'' ;
BQ < '`' ;
COMMA < ',' ;

quote <-- SQ _ s-exp ;
quasiquote <-- BQ _ s-exp ;
unquote <-- COMMA _ s-exp ;
