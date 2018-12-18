#lang racket

(require rackunit)
(require peg)
(require "peg-syntax/peg-example-quests.rkt")

(check-equal? (peg quest "quest firstLevel
		   exec ls -l ;
		   echo You type the correct command ;
		   tseuq")
	      (quest
		(identifier-quest "firstLevel")
		(seqComand
			(exec
				(ls-struct "-l" '()))
			(echo-struct "You type the correct command "))
		'()))
