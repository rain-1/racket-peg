#lang peg

import peg-example-expr.rkt ;


//based on grammar
//https://github.com/ChristianoBraga/BPLC/blob/python-ibm-cloud/examples/imp/doc/imp.pdf

//to see a old 'broken' implementation, see
//https://github.com/Grupo-de-compiladores/apresenta-o-de-racket

//is in brazilian portuguese, the name is "racket's presentation"



program <-- _ 'module' _ identifier _ clauses _ procs _ 'end' _ EOI ;
_ < [ \t\n]* ;
identifier <- [a-zA-Z]+ ;
clauses <-- vars? _ const? _ init? ;
vars <-- 'var' _ identifier _ (',' _ identifier _)* ';' ;
const <-- 'const' _ identifier _ (',' _ identifier _)* ';' ;
init <--  'init' _ initialization _ (',' _ initialization _)* ';' ;
initialization <-- identifier _ '=' _ expr ;
procs <-- proc (_ proc)* ;
proc <-- 'proc' _ identifier _ block ; //yes proc can have arguments, KISS guy, KISS.
block <-- '{' _ clauses? _ command (_ command)* _ '}' ; //this is wrong, because in IMP,
                                                        //declarations inside block is different,
                                                        //but to simplify the grammar,
                                                        //I will use the same clauses.
                                                        
command <-- (unitary-command _ command) / unitary-command ;
unitary-command <-- atribution / print / exit ; //yes, ';' is operator to seq commands, but to
                                                //simplify all command end with this
atribution <-- identifier _ ':=' _ expr _ ';' ;
print <-- 'print' _ expr _ ';' ;
exit <-- 'exit' _ expr _ ';' ;



EOI < ! . ;
