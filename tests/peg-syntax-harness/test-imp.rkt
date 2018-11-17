(use-modules (racket-peg peg))
(use-modules (racket-peg guile-heredoc))
(use-modules (racket-peg rackunit))

#<<PEG


expr <- sum ;
sum <-- (product ('+' / '-') sum) / product ;
product <-- (value ('*' / '/') product) / value ;
value <-- number / '(' expr ')' ;
number <-- [0-9]+ ;


//based on grammar
//https://github.com/ChristianoBraga/BPLC/blob/python-ibm-cloud/examples/imp/doc/imp.pdf

//to see a old 'broken' implementation, see
//https://github.com/Grupo-de-compiladores/apresenta-o-de-racket

//is in brazilian portuguese, the name is "racket's presentation"



program <-- _ 'module' _ identifier _ clauses _ procs _ 'end' _ EOI ;
_ < [ \t\n]* ;
identifier <-- [a-zA-Z]+ ;
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

PEG

(check-equal?
  (peg program "


  module test

  var x,y,z ;
  const w,h,j ;
  init x=0,y=1,z=4,w=10,h=15,j=100;

  proc main
  {
         print 15 ;
  }


  end")
  '(program
  "module"
  (identifier . "test")
  (clauses
   (vars "var" (identifier . "x") "," (identifier . "y") "," (identifier . "z") ";")
   (const "const" (identifier . "w") "," (identifier . "h") "," (identifier . "j") ";")
   (init "init"
         (initialization (identifier . "x") "=" (sum product value number . "0"))
         ","
         (initialization (identifier . "y") "=" (sum product value number . "1"))
         ","
         (initialization (identifier . "z") "=" (sum product value number . "4"))
         ","
         (initialization (identifier . "w") "=" (sum product value number . "10"))
         ","
         (initialization (identifier . "h") "=" (sum product value number . "15"))
         ","
         (initialization (identifier . "j") "=" (sum product value number . "100"))
         ";"))
  (procs (proc "proc" (identifier . "main") (block "{" (clauses) (command unitary-command print "print" (sum product value number . "15") ";") "}")))
  "end"))
