#lang racket

(require peg)
(require rackunit)

(require "peg-example-imp.rkt")

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
