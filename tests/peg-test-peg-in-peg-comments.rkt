#lang racket

(require peg)
(require rackunit)

(require "peg-example-peg-in-peg-comments.rkt")

(check-equal? (peg number "123")
    '(number . "123")) ; 123 in decimal
    
(check-equal? (peg number "1024")
    '(number . "1024")) ; 1024 in decimal
    
(check-equal? (peg number "0123")
    '(number . "0123"))  ; 0123 is a octal, 83 decimal. In the way that I put, don't matter.
    
(check-not-equal? (peg number "0x1024")
    '(number . "1024"))
    
(check-equal? (peg number "0x1024")
    '(number . "0"))
      ; what the hell? Simple : the parser match 0, but x is not a digit, so stop.
      ; this is important : the parser will try to parse so long as he get, but don't
      ; will find our desires.
     
#|
  the comments in the parser don't show up in the parsed structure.
  they are just ignored by the peg

|#
