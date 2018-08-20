#lang peg

(require "peg-example-float.rkt") ;

// based on https://notes.kartashov.com/2016/01/30/writing-a-simple-query-language-with-antlr/

_ <- [ ]* ;

OP < '(' _ ;
CL < ')' _ ;
CM < ',' _ ;

location <-- OP float CM float CL ;
amount <-- float _ unit ;
unit <- 'km' _ / '%' _ ;

expression <- predicate 'and' _ expression
	    / predicate 'or' _ expression
            / predicate ;
predicate <-- 'location' _ 'within' _ amount 'from' _ location
           / 'status.stateOfCharge' '<' _ amount ;
