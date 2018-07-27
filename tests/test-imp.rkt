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
  ''(program
  "moduletest"
  (clauses
   (vars . "varx,y,z;")
   (const . "constw,h,j;")
   (init "init"
         (initialization "x=" (sum product value number . "0"))
         ","
         (initialization "y=" (sum product value number . "1"))
         ","
         (initialization "z=" (sum product value number . "4"))
         ","
         (initialization "w=" (sum product value number . "10"))
         ","
         (initialization "h=" (sum product value number . "15"))
         ","
         (initialization "j=" (sum product value number . "100"))
         ";"))
  (procs (proc "procmain" (block "{" (clauses) (command unitary-command print "print" (sum product value number . "15") ";") "}")))
  "end"))
  
  //we can match correct programs, but obvialy the struct is ugly yet

