#lang racket

(require rackunit)
(require peg)

(define-peg number (name v (+ (range #\0 #\9))) (string->number v))
(define-peg sum (and (name v1 number) "+" (name v2 number)) (+ v1 v2))


;because in non-debug mode, nothing is printed, just returned
(check-equal? "" (with-output-to-string (lambda () (peg sum "12+123"))))


;the result is 135, showed in the last line. ALL before is debug
;(check-equal? ">(peg-rule:local-debug #<procedure:peg-result->object>)\n>(peg-rule:sum-debug #<procedure:sk^>)\n>(peg-rule:number-debug #<procedure:sk^>)\n>(peg-rule:number-debug #<procedure:sk^>)\n<135\n" (with-output-to-string (lambda () (peg sum "12+123" #t))))

;> (peg (and sum (and "," sum)) "12+123,55+33" #t)
;peg-rule:local
; peg-rule:sum
;  peg-rule:number
;   peg-rule:number
;    peg-rule:sum
;     peg-rule:number
;      peg-rule:number
;'(135 "," 88)
