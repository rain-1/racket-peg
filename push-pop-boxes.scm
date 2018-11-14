(define-module (racket-peg push-pop-boxes)
  #:use-module (srfi srfi-111)
  #:export (push! pop!))

;;;;
;; push and pop for boxes

(define-syntax push!
  (syntax-rules ()
    ((push! place thing)
     (set-box! place (cons thing (unbox place))))))

(define-syntax pop!
  (syntax-rules ()
    ((pop! place)
     (let ((top (car (unbox place))))
       (set-box! place (cdr (unbox place)))
       top))))

;scheme@(guile-user)> (load "push-pop-boxes.rkt")
;scheme@(guile-user)> (use-modules (racket-peg push-pop-boxes) (srfi srfi-111))
;scheme@(guile-user)> (let ((x (box '()))) (push! x 3) x)
;$1 = #<box 55c5617c2c80 value: (3)>
