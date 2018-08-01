#lang peg

_ < [ \t\n]*;

s-exp <-
  list /
  quote / quasiquote / unquote /
  boolean / number / identifier / string;

list <- '(' _ lst:(s-exp _)* ')' -> lst;

quote      <- '\'' _ s:s-exp -> (list 'quote s);
quasiquote <- '`' _ s:s-exp  -> (list 'quasiquote s);
unquote    <- ',' _ s:s-exp  -> (list 'unquote s);

boolean    <- x:'#t' / x:'#f'               -> (equal? "#t" x);
identifier <- s:[^ ()\[\]{}",'`;#|\\]+      -> (string->symbol s);
number     <- n:[0-9]+                      -> (string->number n);
string     <- ["] s:([^"\\] / ~[\\] .)* ["] -> s;
