#lang racket

(provide seq-elt seq-elt-object
         seq-cat seq-cat-subseqs
         seq?
         empty-sequence empty-sequence?
         seq->dlist seq->list
         
         peg-result peg-result-str
         
         peg-result-join
         peg-result->object)

;;;;
;; sequences

(struct seq-elt (object) #:transparent)
(struct seq-cat (subseqs) #:transparent)
(define (seq? x) (or (seq-elt? x) (seq-cat? x)))

(define empty-sequence (seq-cat '()))
(define (empty-sequence? x)
  ;; *** you could cat multiple empty sequences together but that is not
  ;; what this function cares about
  (and (seq-cat? x) (null? (seq-cat-subseqs x))))

(define (seq->dlist seq tail)
  (cond ((seq-elt? seq) (cons (seq-elt-object seq) tail))
	((seq-cat? seq) (foldr seq->dlist tail (seq-cat-subseqs seq)))
	(else (error "seq->dlist failed" seq))))
(define (seq->list seq) (seq->dlist seq '()))

;;;;
;; peg results

(struct peg-result (str) #:transparent)

(define (peg-result-append x y)
  (peg-result (string-append (peg-result-str x) (peg-result-str y))))

(define (peg-result-join x y)
  (cond ((and (seq? x) (seq? y))
         (seq-cat (list x y)))
        ((seq? y)
         (seq-cat (list (seq-elt x) y)))
        ((seq? x)
         (seq-cat (list x (seq-elt y))))
        (else
         (seq-cat (list (seq-elt x) (seq-elt y))))))
        
(define (peg-result->object x)
  (cond ((peg-result? x) (peg-result-str x))
        ((seq? x) (concat-peg-result-strings #t (seq->list x)))
        (else x)))

(define (concat-peg-result-strings singletonize? lst)
  ;; singletonization should only apply to a length one sequence containing a peg-result
  ;; because we want kleene star of a value producer to make a list of values (even when there is only 1)
  ;; but what about (and x y) where x produces a value and y produces the empty sequence?
  (cond ((null? lst)
         '())
        ((and (null? (cdr lst))
              singletonize?
              (peg-result? (car lst)))
         (peg-result-str (car lst)))
        ((and (peg-result? (car lst))
              (peg-result? (cadr lst)))
         (concat-peg-result-strings singletonize? (cons (peg-result-append (car lst) (cadr lst)) (cddr lst))))
        (else
         (cons (car lst) (concat-peg-result-strings #f (cdr lst))))))

