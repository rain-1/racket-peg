(define-module (racket-peg peg-result)
  #:use-module (srfi srfi-9)
  #:export (seq-elt seq-elt-object
		    seq-cat seq-cat-subseqs
		    seq?
		    empty-sequence empty-sequence?
		    seq->dlist seq->list
		    
		    peg-result peg-result-str
		    
		    peg-result-join
		    peg-result->object))

;;;;
;; sequences

(define-record-type <seq-elt> (seq-elt object) seq-elt?
		    (object seq-elt-object))
(define-record-type <seq-cat> (seq-cat subseqs) seq-cat?
		    (subseqs seq-cat-subseqs))
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


#|

;; Just basic PEG rules

By default the result of a PEG rule will be the string it matched.
To implement this we make a sequence of peg-result objects, so for example:

the result of matching the rule
   (seq #\f "oo")
on the input "foo" would give
   (seq-cat (list (seq-elt "f") (seq-elt "oo")))
which represents
   "foo"

The function `peg-result->object` turns the representation back into the string,
it does this by transforming the sequence into a list then appending all the
peg-result string objects that are next to each other in the list.


;; Mixing in semantic actions

The PEG results produced by operators like `and` and `*` build final results out of
partial results using `peg-result-join`. At the end of the parse this can be converted
into a regular scheme object using `peg-result->object`.

The implementation is complicated by the fact that PEG parsers can also produce scheme
values using semantic actions. For example you might make a `number` rule that matches
strings like `"35"` but producing as its result the number `35`, not just the string.
In that case we need `(* number)` on `"35 66 72"` to produce a list of numbers.

To achieve this our `peg-result-join` operation has to
* join sequences together
* cons/snoc regular scheme objects onto existing sequences

Secondly, `peg-result->object` can no longer just join all the peg-result strings in the
result sequence together. It has to leave scheme objects alone!

It also has a new singletonization rule: A length 1 sequence containing a peg-result string
represents a string, not a length 1 list with a string in it.

|#

(define-record-type <peg-result> (peg-result str) peg-result?
		    (str peg-result-str))

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
  (cond ((null? lst)
         '())
        ((and (null? (cdr lst))
              singletonize?
              (peg-result? (car lst)))
         (peg-result-str (car lst)))
        ((and (not (null? (cdr lst)))
              (peg-result? (car lst))
              (peg-result? (cadr lst)))
         (concat-peg-result-strings singletonize? (cons (peg-result-append (car lst) (cadr lst)) (cddr lst))))
        (else
         (cons (peg-result->object (car lst))
               (concat-peg-result-strings #f (cdr lst))))))


