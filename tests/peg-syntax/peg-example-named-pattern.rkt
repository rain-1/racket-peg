#lang peg

number <- value:[0-9]+ -> (string->number value);
sum <- v1:number ~'+' v2:number -> `(+ ,v1 ,v2);
