#lang peg

(require "peg-example-shell.rkt");


(provide (all-from-out "peg-example-shell.rkt"));

(define (parser-quest port)
  (peg top-quest (port->string port)));

(struct quest (name command preRequisites) #:transparent);

(struct seqComand (a b) #:transparent);

(struct identifier-quest (a) #:transparent);
(struct exec (c) #:transparent);
(struct file (file assertion) #:transparent);

(define (red l f)
  (if (null? (cdr l))
      (car l)
      (f (car l) (red (cdr l) f))));




_ < [ \t\n]* ;
sep < [ \t]* ;
EOI < ! . ;

top-quest <- _ v:quest _ EOI -> v;
quest <- quest-with-pre-requisites / quest-without-pre-requisites;

quest-with-pre-requisites <- _ 'quest' _ v:identifier _ p:preRequisites _ c:comand _ 'tseuq' _ -> (quest v c p);
quest-without-pre-requisites <- _ 'quest' _ v:identifier _ c:comand _ 'tseuq' _ -> (quest v c (list));
identifier <- v:[a-zA-Z]+ -> (identifier-quest v);
preRequisites <- '<' _ l:(identifier _ (~',' _ identifier _)*) -> l;
comand <- v:(comandUnit (_ comandUnit)*) -> (red v seqComand); //this create a sequence of commands associative from right
comandUnit <- v:(exec / echo-quest / file) ';' -> v;
exec <- 'exec' sep c:shell -> (exec c);
echo-quest <- echo ;
file <- 'file' sep v:identifier sep k:assertionOnFile -> (file v k);
assertionOnFile <- exists / is-directory ;
exists <- 'exists' -> 'exists;
is-directory <- 'is directory' -> 'is-directory;



//quest ls > cd,mkdir

//exec ls;

//echo Very good, this is your directory;

//file ola exists;

//exec cd p;

//tseuq



