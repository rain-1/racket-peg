#lang peg

(require "peg-example-shell.rkt");

(define (parser-quest port)
  (peg top-quest (port->string port)));


_ < [ \t\n]* ;
sep < [ \t]* ;
EOI < ! . ;

top-quest <- _ quest _ EOI ;
quest <- _ 'quest' _ identifier _ preRequisites? _ comand _ 'tseuq' _ ;
identifier <- [a-zA-Z]+ ;
preRequisites <- '<' _ identifier _ (',' _ identifier _)*;
comand <- comandUnit (_ comandUnit)* ;
comandUnit <- (exec / echo-quest / file) ';' ;
exec <- 'exec' sep shell ;
echo-quest <- echo ;
file <- 'file' sep identifier sep assertionOnFile ;
assertionOnFile <- 'exists' / 'is directory' ;



//quest ls > cd,mkdir

//exec ls

//echo "muito bem, você mostrou o diretorio"

//file ola exists

//exec cd p

//tseuq



