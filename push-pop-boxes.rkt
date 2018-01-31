#lang racket

(provide push! pop!)

;;;;
;; push and pop for boxes

(define-syntax-rule (push! place thing)
  (set-box! place (cons thing (unbox place))))

(define-syntax-rule (pop! place)
  (let ((top (car (unbox place))))
    (set-box! place (cdr (unbox place)))
    top))
