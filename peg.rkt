#lang racket

(provide define-peg
         define-peg/bake define-peg/drop define-peg/tag
         peg)

(require (for-syntax racket/syntax))
(require racket/trace)
(require "push-pop-boxes.rkt")
(require "peg-result.rkt")

;;;;
;; generic utility functions

(define (char->string ch) (list->string (list ch)))
(define (string-contains-char? str ch) (string-contains? str (char->string ch)))
(define (string-contains-substring? str place contained)
  (and (>= (string-length str) (+ place (string-length contained)))
       (string=? (substring str place (+ place (string-length contained)))
                 contained)))
(define (char-between? x c1 c2)
  (and (<= (char->integer c1) (char->integer x))
       (<= (char->integer x) (char->integer c2))))

;;;;
;; peg s-exp syntax

;; <peg> ::= (epsilon)
;;         | (char c) | c
;;         | (any-char)
;;         | (range c1 c2)
;;         | (string str) | str
;;         | (and <peg> <peg> ...)
;;         | (or <peg> <peg> ...)
;;         | (* <peg>)
;;         | (+ <peg>)
;;         | (? <peg>)
;;         | (call nm) | nm
;;         | (name nm <peg>)
;;         | (! <peg>)
;;         | (& <peg>)
;;         | (drop <peg>)


;;;;
;; pegvm registers and dynamic control


(define pegvm-verbose (make-parameter #f))
(define pegvm-input-text (make-parameter #f))      ;; The input string being parsed
(define pegvm-input-position (make-parameter #f))  ;; The position inside the string
(define pegvm-control-stack (make-parameter #f))   ;; For or control flow
(define pegvm-stashed-stacks (make-parameter #f))  ;; For negation control flow
(define pegvm-negation? (make-parameter #f))       ;; How many negations have we entered

(define pegvm-best-failure (make-parameter #f))    ;; For error messages
(define pegvm-current-rule (make-parameter #f))
(define pegvm-current-choice (make-parameter #f))

(struct control-frame (position label) #:transparent)

(define (pegvm-eof?) (= (string-length (pegvm-input-text)) (unbox (pegvm-input-position))))
(define (pegvm-peek) (string-ref (pegvm-input-text) (unbox (pegvm-input-position))))
(define (pegvm-advance! n) (set-box! (pegvm-input-position) (+ (unbox (pegvm-input-position)) n)))
(define (pegvm-push-alternative! alt) (push! (pegvm-control-stack) (control-frame (unbox (pegvm-input-position)) alt)))
(define (pegvm-fail)
  (if (pegvm-verbose)
	(begin
	(display (format "FAIL!!!!!!! On Rule ~a\n" (pegvm-current-rule)))
	(parameterize ((pegvm-verbose (sub1 (pegvm-verbose))))
	  (pegvm-update-best-error!)
	  (let ((cf (pop! (pegvm-control-stack))))
	    (when (control-frame-position cf)
	      (set-box! (pegvm-input-position) (control-frame-position cf)))
	    ((control-frame-label cf)))))
	(begin
	  (pegvm-update-best-error!)
	  (let ((cf (pop! (pegvm-control-stack))))
	    (when (control-frame-position cf)
	      (set-box! (pegvm-input-position) (control-frame-position cf)))
	    ((control-frame-label cf))))))
(define (pegvm-enter-negation!)
  (set-box! (pegvm-negation?) (+ (unbox (pegvm-negation?)) 1))
  (push! (pegvm-stashed-stacks) (unbox (pegvm-control-stack)))
  (set-box! (pegvm-control-stack) '()))
(define (pegvm-exit-negation!)
  (set-box! (pegvm-negation?) (- (unbox (pegvm-negation?)) 1))
  (set-box! (pegvm-control-stack) (pop! (pegvm-stashed-stacks))))

(define (pegvm-update-best-error!)
  (when (or (not (unbox (pegvm-best-failure)))
            (> (unbox (pegvm-input-position)) (car (unbox (pegvm-best-failure)))))
    (let ((pos (unbox (pegvm-input-position))))
      (set-box! (pegvm-best-failure) (list pos (pegvm-current-rule) (pegvm-current-choice))))))

(define (calculate-line-and-column str pos)
  (let ((line 1)
	(col 0))
    (for ((chr (in-string str 0 pos)))
	 (if (equal? chr #\newline)
	     (begin (set! line (+ line 1))
		    (set! col 0))
	     (begin (set! col (+ 1 col)))))
    `(line ,line column ,col)))

;;;;
;; peg compiler

(define-for-syntax (peg-names exp)
  (syntax-case exp (epsilon char any-char range string and or * + ? call name ! drop)
    [(and e1) (peg-names #'e1)]
    [(and e1 e2) (append (peg-names #'e1) (peg-names #'e2))]
    [(and e1 e2 . e3) (append (peg-names #'e1) (peg-names #'(and e2 . e3)))]
    [(or e1 ...) (peg-names #'(and e1 ...))]
    [(* e1 ...) (peg-names #'(and e1 ...))]
    [(+ e1 ...) (peg-names #'(and e1 ...))]
    [(? e1 ...) (peg-names #'(and e1 ...))]
    [(name nm subexp) (cons #'nm (peg-names #'subexp))]
    [(drop e1 ...) (peg-names #'(and e1 ...))]
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
    (syntax-case exp (epsilon char any-char range string and or * + ? call name ! drop
                              $or)
      [(epsilon)
       #'(sk empty-sequence)]
      [(char c)
       (single-char-pred #'sk #'x #'(char=? c x))]
      [(any-char)
       (single-char-pred #'sk #'x #t)]
      [(range c1 c2)
       (single-char-pred #'sk #'x #'(char-between? x c1 c2))]
      [(string str)
       (with-syntax ([str-len (string-length (syntax->datum #'str))])
         #'(if (string-contains-substring? (pegvm-input-text) (unbox (pegvm-input-position)) str)
               (begin (pegvm-advance! str-len)
                      (sk (peg-result str)))
               (pegvm-fail)))]
      [(and e1)
       (peg-compile #'e1 #'sk)]
      [(and e1 e2)
       (with-syntax ([p1 (peg-compile #'e1 #'mk)]
                     [p2 (peg-compile #'e2 #'sk^)])
         #'(let ((stack-reset (unbox (pegvm-control-stack))))
             (let ((mk (lambda (r1)
                         (let ((sk^ (lambda (r2)
					(if (pegvm-verbose)
						(parameterize ((pegvm-verbose (sub1 (pegvm-verbose))))
		                                      (sk (peg-result-join r1 r2)))
						(sk (peg-result-join r1 r2))))))
                           (set-box! (pegvm-control-stack) stack-reset)
                           p2))))
              p1
		)))]
      [(and e1 e2 e3 ...)
       (peg-compile #'(and e1 (and e2 e3 ...)) #'sk)]
;      [(and e1 e2 e3 ...)
;       (peg-compile #'(and (and e1 e2) e3 ...) #'sk)]
      [($or e1 e2)
       (with-syntax ([p1 (peg-compile #'e1 #'sk)]
                     [p2 (peg-compile #'e2 #'sk)])
         #'(begin (pegvm-push-alternative! (lambda () p2))
                  p1))]
      [(or e1)
       (peg-compile #'e1 #'sk)]
      [(or e1 e2)
       (with-syntax ([p (peg-compile #'($or e1 e2) #'sk)])
         #'(parameterize ([pegvm-current-choice '(or e1 e2)])
             p))]
      [(or e1 e2 e3 ...)
       (with-syntax ([p (peg-compile #'($or e1 (or e2 e3 ...)) #'sk)])
         #'(parameterize ([pegvm-current-choice '(or e1 e2 e3 ...)])
             p))]
      [(* e)
       (with-syntax ([p (peg-compile #'e #'s+)])
         #'(letrec ((s* (lambda (res-acc)
                          (pegvm-push-alternative! (lambda () (sk res-acc)))
                          (let ((s+ (lambda (res)
                                      (s* (peg-result-join res-acc res)))))
                            p))))
             (s* empty-sequence)))]
      [(* e1 e2 ...)
       (peg-compile #'(* (and e1 e2 ...)) #'sk)]
      [(+ e)
       (peg-compile #'(and e (* e)) #'sk)]
      [(+ e1 e2 ...)
       (peg-compile #'(+ (and e1 e2 ...)) #'sk)]
      [(? e)
       (with-syntax ([p (peg-compile #'e #'sk)])
         #'(begin (pegvm-push-alternative! (lambda () (sk empty-sequence)))
                  p))]
      [(? e1 e2 ...)
       (peg-compile #'(? (and e1 e2 ...)) #'sk)]
      [(call rule-name)
       (with-syntax ([rule (format-id #'rule-name "peg-rule:~a" #'rule-name)])
		    #'(if (pegvm-verbose)
			  (parameterize ((pegvm-verbose (+ (pegvm-verbose) 1)))
			    (rule sk))
			  (rule sk)))]
      [(name nm e)
       (with-syntax ([p (peg-compile #'e #'sk^)])
         #'(let ((sk^ (lambda (r)
                        (when (= 0 (unbox (pegvm-negation?)))
                          (set! nm (peg-result->object r)))
                        (sk r))))
             p))]
;      [(transform e code)
;       (with-syntax ([p (peg-compile #'e #'sk^)])
;         #'(let ((sk^ (lambda (r)
;                        (sk code))))
;             p))]
      [(! e)
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
      [(! e1 e2 ...)
       (peg-compile #'(and (! e1) (! e2) ...) #'sk)]
      [(drop e)
       (with-syntax ([p (peg-compile #'e #'sk^)])
         #'(let ((sk^ (lambda (_) (sk empty-sequence))))
             p))]
      [(drop e1 e2 ...)
       (peg-compile #'(drop (and e1 e2 ...)) #'sk)]
      [(& e) (peg-compile #'(! (! e)) #'sk)]
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
     #'(define-peg rule-name exp #f #f)]
    [(_ rule-name exp action)
     #'(define-peg rule-name exp action #t)]
    [(_ rule-name exp action has-action?)
     (with-syntax ([name (format-id #'rule-name "peg-rule:~a" (syntax-e #'rule-name))]
                   [bindings (map make-binding (peg-names #'exp))]
                   [body (peg-compile #'exp #'sk^)]
                   [action (if (syntax-e #'has-action?) #'action #'res)])
       #'(define (name sk)
	   (when (pegvm-verbose)
	     (display (make-string (pegvm-verbose) #\space))
	     (display 'name)
	     (newline))
           (parameterize ([pegvm-current-rule 'name])
	     (let* bindings
	       (let ((sk^ (lambda (res)
				(if (pegvm-verbose)
					(parameterize ((pegvm-verbose (sub1 (pegvm-verbose))))
						(sk action))
					(sk action)))))
		
		(if (pegvm-verbose)
			(parameterize ((pegvm-verbose (add1 (pegvm-verbose))))
		         body)
			body)
		
		)))))]))

(define-syntax (define-peg/drop stx)
  (syntax-case stx () [(_ rule-name exp) #'(define-peg rule-name (drop exp))]))

(define-syntax (define-peg/bake stx)
  (syntax-case stx () [(_ rule-name exp) #'(define-peg rule-name (name res exp) res)]))

(define-syntax (define-peg/tag stx)
  (syntax-case stx () [(_ rule-name exp) #'(define-peg rule-name (name res exp) (cons 'rule-name res))]))

(define (copy-and-pad-substring str a b)
  ;; pad with spaces
  (let ((res (make-string (- b a) #\space))
        (len (string-length str)))
    (let ((left-padding (max 0 (- a)))) ;; are we going off the left end of the string (is a negative)
      (string-copy! res left-padding str (max 0 a) (min len b))
      res)))

;(check-equal? (copy-and-pad-substring "abcdefghijklmnopqrstuvwxyz" -2 3)
;              "  abc")
;(check-equal? (copy-and-pad-substring "abcdefghijklmnopqrstuvwxyz" -1 3)
;              " abc")
;(check-equal? (copy-and-pad-substring "abcdefghijklmnopqrstuvwxyz" 0 3)
;              "abc")
;(check-equal? (copy-and-pad-substring "abcdefghijklmnopqrstuvwxyz" 1 3)
;              "bc")
;(check-equal? (copy-and-pad-substring "abcdefghijklmnopqrstuvwxyz" 2 3)
;              "c")
;(check-equal? (copy-and-pad-substring "abcdefghijklmnopqrstuvwxyz" 26 30)
;              "    ")
;(check-equal? (copy-and-pad-substring "abcdefghijklmnopqrstuvwxyz" 25 30)
;              "z    ")
;(check-equal? (copy-and-pad-substring "abcdefghijklmnopqrstuvwxyz" 24 30)
;              "yz    ")

(define-syntax (peg stx)
  (syntax-case stx ()
    [(_ exp str) #'(peg exp str #f)]
    [(_ exp str v)
     #'(let ((fail-cont (lambda ()
                          (let ((loc (car (unbox (pegvm-best-failure)))))
                            (display (string-replace (string-replace (copy-and-pad-substring (pegvm-input-text) (- loc 10) (+ loc 10))
                                                                     "\n" "N") "\t" "T"))
                            (newline)
                            (display "          ^ here")
                            (newline))
                          (error 'peg "parse failed in rule ~a at location ~a with options ~v"
                                 (cadr (unbox (pegvm-best-failure)))
				 (calculate-line-and-column (pegvm-input-text) (car (unbox (pegvm-best-failure))))
                                 (caddr (unbox (pegvm-best-failure)))
                                 )))
             (success-cont peg-result->object))
         (define-peg local exp)
         (parameterize ([pegvm-input-text str]
                        [pegvm-input-position (box 0)]
                        [pegvm-control-stack (box (list (control-frame #f fail-cont)))]
                        [pegvm-stashed-stacks (box '())]
                        [pegvm-negation? (box 0)]
                        [pegvm-best-failure (box #f)]
			[pegvm-verbose (if v 0 #f)])
	   (peg-rule:local success-cont)))]))


