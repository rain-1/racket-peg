#lang racket

(provide seq-elt seq-elt-object
         seq-cat seq-cat-subseqs
         seq?
         empty-sequence empty-sequence?
         seq->dlist seq->list
         peg-result peg-result-str
         peg-result->object
         peg-result-join)

;;;;
;; sequences

;; by default a peg parse will return the matched string
;;   e.g. (star #\A) "AAAA!" should return "AAAA"
;;
;; but users can define semantic actions that produce other kinds of scheme objects
;;   for example if you defined a parser for numbers that produced a single number
;;   (star number) "32 55 72" should return a sequence (32 55 72)
;;
;; this means we need some machinary for the results of a parse to get automatically
;; joined together. so we define 2 data types:
;; * joinable sequences
;; * peg-result type - holds a string, but it's a distinct type from string
;;
;; and it needs to have a whole series of rules about automatic joining
;; - any two peg-result strings next to each other should be automatically joined into one
;; - joining a peg-result to a sequence should act like 'cons'
;; - the reverse of that should act like 'snoc'
;; - joining two sequences joins them
;; - a singleton sequence containing a peg-result string => single string object

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

(struct peg-result (str) #:transparent)

(define (peg-result->object x)
  (cond ((peg-result? x) (peg-result-str x))
        ((seq? x)
         (cat-peg-results (seq->list x) #t))
        (else x)))

(define (peg-result-join x y)
  (cond ((empty-sequence? x) y)
        ((empty-sequence? y) x)
        ((and (seq? x) (seq? y))
         (seq-cat (list x y)))
        ((seq? x)
         (seq-cat (list x (seq-elt y))))
        ((seq? y)
         (seq-cat (list (seq-elt x) y)))
        (else
         (seq-cat (list (seq-elt x) (seq-elt y))))))

(define (cat-peg-results lst singletonize?)
  (cond ((null? lst) lst)
        ((and (null? (cdr lst)) (peg-result? (car lst)))
         (if singletonize?
             (peg-result-str (car lst))
             (list (peg-result-str (car lst)))))
        ((null? (cdr lst)) lst)
        ((and (peg-result? (car lst))
              (peg-result? (cadr lst)))
         (cat-peg-results (cons (peg-result (string-append (peg-result-str (car lst))
                                                           (peg-result-str (cadr lst))))
                                (cddr lst))
                          #t))
        ((peg-result? (car lst))
         (cons (peg-result-str (car lst)) (cat-peg-results (cdr lst) #f)))
        (else (cons (car lst) (cat-peg-results (cdr lst) #f)))))
