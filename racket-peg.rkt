#lang racket

(provide define-peg peg)

(require (for-syntax racket/syntax))

(require "push-pop-boxes.rkt")
(require "peg-sequences.rkt")

;;;;
;; generic utility functions

(define (char->string ch) (list->string (list ch)))
(define (string-contains-char? str ch) (string-contains? str (char->string ch)))
(define (string-contains-substring? str place contained)
  (and (>= (string-length str) (+ place (string-length contained)))
       (string=? (substring str place (+ place (string-length contained)))
                 contained)))

;;;;
;; peg s-exp syntax

;; <peg> ::= (epsilon)
;;         | (char c)
;;         | (any-char)
;;         | (range str)
;;         | (string str)
;;         | (seq <peg> <peg>)
;;         | (choice <peg> <peg>)
;;         | (star <peg>)
;;         | (plus <peg>)
;;         | (optional <peg>)
;;         | (call nm)
;;         | (name nm <peg>)
;;         | (not <peg>)


;;;;
;; pegvm registers and dynamic control

(define pegvm-input-text (make-parameter #f))      ;; The input string being parsed
(define pegvm-input-position (make-parameter #f))  ;; The position inside the string
(define pegvm-control-stack (make-parameter #f))   ;; For choice control flow
(define pegvm-stashed-stacks (make-parameter #f))  ;; For negation control flow
(define pegvm-negation? (make-parameter #f))       ;; How many negations have we entered

(struct control-frame (position label) #:transparent)

(define (pegvm-eof?) (= (string-length (pegvm-input-text)) (unbox (pegvm-input-position))))
(define (pegvm-peek) (string-ref (pegvm-input-text) (unbox (pegvm-input-position))))
(define (pegvm-advance! n) (set-box! (pegvm-input-position) (+ (unbox (pegvm-input-position)) n)))
(define (pegvm-push-alternative! alt) (push! (pegvm-control-stack) (control-frame (unbox (pegvm-input-position)) alt)))
(define (pegvm-fail)
  (let ((cf (pop! (pegvm-control-stack))))
    (when (control-frame-position cf)
      (set-box! (pegvm-input-position) (control-frame-position cf)))
    ((control-frame-label cf))))
(define (pegvm-enter-negation!)
  (set-box! (pegvm-negation?) (+ (unbox (pegvm-negation?)) 1))
  (push! (pegvm-stashed-stacks) (unbox (pegvm-control-stack)))
  (set-box! (pegvm-control-stack) '()))
(define (pegvm-exit-negation!)
  (set-box! (pegvm-negation?) (- (unbox (pegvm-negation?)) 1))
  (set-box! (pegvm-control-stack) (pop! (pegvm-stashed-stacks))))


;;;;
;; peg compiler

(define-for-syntax (peg-names exp)
  (syntax-case exp (epsilon char any-char range string seq choice star plus optional call name not)
    [(seq e1 e2) (append (peg-names #'e1) (peg-names #'e2))]
    [(seq e1 e2 . e3) (append (peg-names #'e1) (peg-names #'(seq e2 . e3)))]
    [(choice e1 e2) (append (peg-names #'e1) (peg-names #'e2))]
    [(choice e1 e2 . e3) (append (peg-names #'e1) (peg-names #'(choice e2 . e3)))]
    [(star e1) (peg-names #'e1)]
    [(plus e1) (peg-names #'e1)]
    [(optional e1) (peg-names #'e1)]
    [(name nm subexp) (cons #'nm (peg-names #'subexp))]
    [else '()]))

(define-for-syntax (peg-compile exp sk)
  (define (single-char-pred sk x cond)
    (with-syntax ([sk sk] [x x] [cond cond])
      #'(if (pegvm-eof?)
            (pegvm-fail)
            (let ((x (pegvm-peek)))
              (if cond
                  (begin (pegvm-advance! 1)
                         (sk (peg-result (char->string x))))
                  (pegvm-fail))))))
  (with-syntax ([sk sk])
    (syntax-case exp (epsilon char any-char range string seq choice star plus optional call name not)
      [(epsilon)
       #'(sk empty-sequence)]
      [(char c)
       (single-char-pred #'sk #'x #'(char=? c x))]
      [(any-char)
       (single-char-pred #'sk #'x #t)]
      [(range str)
       (single-char-pred #'sk #'x #'(string-contains-char? str x))]
      [(string str)
       (with-syntax ([str-len (string-length (syntax->datum #'str))])
         #'(if (string-contains-substring? (pegvm-input-text) (unbox (pegvm-input-position)) str)
               (begin (pegvm-advance! str-len)
                      (sk (peg-result str)))
               (pegvm-fail)))]
      [(seq e1 e2)
       (with-syntax ([p1 (peg-compile #'e1 #'mk)]
                     [p2 (peg-compile #'e2 #'sk^)])
         #'(let ((mk (lambda (r1)
                       (let ((sk^ (lambda (r2)
                                    (sk (peg-result-join r1 r2)))))
                         p2))))
             p1))]
      [(seq e1 e2 e3 ...)
       (peg-compile #'(seq e1 (seq e2 e3 ...)) #'sk)]
      [(choice e1 e2)
       (with-syntax ([p1 (peg-compile #'e1 #'sk)]
                     [p2 (peg-compile #'e2 #'sk)])
         #'(begin (pegvm-push-alternative! (lambda () p2))
                  p1))]
      [(choice e1 e2 e3 ...)
       (peg-compile #'(choice e1 (choice e2 e3 ...)) #'sk)]
      [(star e)
       (with-syntax ([p (peg-compile #'e #'s+)])
         #'(letrec ((s* (lambda (res-acc)
                          (pegvm-push-alternative! (lambda () (sk res-acc)))
                          (let ((s+ (lambda (res)
                                      (s* (peg-result-join res-acc res)))))
                            p))))
             (s* empty-sequence)))]
      [(plus e)
       (peg-compile #'(seq e (star e)) #'sk)]
      [(optional e)
       (with-syntax ([p (peg-compile #'e #'sk)])
         #'(begin (pegvm-push-alternative! (lambda () (sk empty-sequence)))
                  p))]
      [(call rule-name)
       (with-syntax ([rule (format-id #'rule-name "peg-rule:~a" #'rule-name)])
         #'(rule sk))]
      [(name nm e)
       (with-syntax ([p (peg-compile #'e #'sk^)])
         #'(let ((sk^ (lambda (r)
                        (when (= 0 (unbox (pegvm-negation?)))
                          (set! nm (peg-result->object r)))
                        (sk r))))
             p))]
      [(not e)
       (with-syntax ([p (peg-compile #'e #'sk^)])
         #'(let ((sk^ (lambda (_)
                        (pegvm-exit-negation!)
                        (pegvm-fail)))
                 (fk (lambda ()
                       (pegvm-exit-negation!)
                       (sk empty-sequence))))
             (pegvm-enter-negation!)
             (pegvm-push-alternative! fk)
             p))]
      [_ (let ((shorthand (syntax-e exp)))
           (cond ((char? shorthand) (peg-compile #`(char #,exp) #'sk))
                 ((string? shorthand) (peg-compile #`(string #,exp) #'sk))
                 ((symbol? shorthand) (peg-compile #`(call #,exp) #'sk))
                 (else (raise-syntax-error "invalid peg" (syntax->datum exp)))))])))

(define-syntax (define-peg stx)
  (define (make-binding nm)
    (with-syntax ([nm nm]) #'(nm #f)))
  (syntax-case stx ()
    [(_ rule-name exp)
     #'(define-peg rule-name exp #f)]
    [(_ rule-name exp action)
     (with-syntax ([name (format-id #'rule-name "peg-rule:~a" #'rule-name)]
                   [bindings (map make-binding (peg-names #'exp))]
                   [body (peg-compile #'exp #'sk^)]
                   [action (if (syntax-e #'action) #'action #'res)])
       #'(define (name sk)
           (let* bindings
             (let ((sk^ (lambda (res) (sk action))))
               body))))]))

(define-syntax (peg stx)
  (syntax-case stx ()
    [(_ rule-name str)
     (with-syntax ([name (format-id #'rule-name "peg-rule:~a" #'rule-name)])
       #'(let ((fail-cont (lambda ()
                            (error "parse failed at location" (unbox (pegvm-input-position)))))
               (success-cont (lambda (res)
                               (display "parse successful! ")
                               (write (peg-result->object res))
                               (newline)
                               res)))
           (parameterize ([pegvm-input-text str]
                          [pegvm-input-position (box 0)]
                          [pegvm-control-stack (box (list (control-frame #f fail-cont)))]
                          [pegvm-stashed-stacks (box '())]
                          [pegvm-negation? (box 0)])
             (name success-cont))))]))


