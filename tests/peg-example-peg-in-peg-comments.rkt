#lang peg

//we can use comments in the start of rules

//we use '<' to mean that the matching string of input is discarted on parser
_ < (' ' / [\t] / [\n])* ;

digit <-- [0-9] ;
number <-- digit+ ; // we can comment on the end of a rule. We can use this to explain that a number is just
                    // a bunch of digits concatenated(minimun of 1 digit)
                    
ident <-- [a-zA-Z] [a-zA-Z0-9_-]* ; // yeah, charclass can be used to indicated ranges of char.

//another use, final comments
