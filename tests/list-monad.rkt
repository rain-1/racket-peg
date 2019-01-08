#lang racket

;; list-monad.rkt

(provide liftm2 sequence
         concatenate string-concatenate)

(define (concatenate lists)
  (apply append lists))

(define (string-concatenate strings)
  (apply string-append strings))

(define (>>= m f)
  (apply append (map f m)))

(define (liftm2 f a b)
  (>>= a (lambda (x) (>>= b (lambda (y) (list (f x y)))))))

(define (sequence lsts)
  (if (null? lsts)
      (list '())
      (liftm2 cons (car lsts) (sequence (cdr lsts)))))
