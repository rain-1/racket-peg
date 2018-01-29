#lang racket

(require (for-syntax racket/syntax))


;;;;
;; generic utility functions

(define (inc! b n)
  (set-box! b (+ (unbox b) n)))

(define (string-contains-substring? str place contained)
  (and (>= (string-length str) (+ place (string-length contained)))
       (string=? (substring str place (+ place (string-length contained)))
                 contained)))

(define (char->string ch) (list->string (list ch)))

(define (string-contains-char? str ch)
  (string-contains? str (char->string ch)))

(define (empty-string? s) (and (string? s) (string=? "" s)))


;;;;
;; push and pop for boxes

(define-syntax-rule (push! place thing)
  (set-box! place (cons thing (unbox place))))

(define-syntax-rule (pop! place)
  (let ((top (car (unbox place))))
    (set-box! place (cdr (unbox place)))
    top))


;;;;
;; peg system registers
;; and convenience functions

(define peg:string (make-parameter #f))
(define peg:sp (make-parameter #f))
(define peg:fail-stack (make-parameter #f))

(define (peg:eof?) (= (unbox (peg:sp)) (- (string-length (peg:string)) 0)))

(define (peg:string@)
  (string-ref (peg:string) (unbox (peg:sp))))

(define (peg:join x y)
  (cond ((empty-string? x) y)
        ((empty-string? y) x)
        ((and (string? x) (string? y)) (string-append x y))
        ((and (list? x) (list? y)) (append x y))
        (else (list x y))))


;;;;
;; peg compiler macro

(define-for-syntax (peg:names exp)
  (syntax-case exp (epsilon char any-char string range seq choice star optional plus call name not)
    [(seq e1 e2) (append (peg:names #'e1) (peg:names #'e2))]
    [(choice e1 e2) (append (peg:names #'e1) (peg:names #'e2))]
    [(star e1) (peg:names #'e1)]
    [(optional e1) (peg:names #'e1)]
    [(plus e1) (peg:names #'e1)]
    
    [(name nm subexp) (cons #'nm (peg:names #'subexp))]

    [else '()]))

(define-for-syntax (peg:compile exp sk)
  (with-syntax ([sk sk])
    (syntax-case exp (epsilon char any-char string range seq choice star optional plus call name not)

      [(epsilon)
       #'(sk "")]
      
      [(char c)
       #'(if (and (not (peg:eof?)) (char=? c (peg:string@)))
             (begin
               (inc! (peg:sp) 1)
               (sk (char->string c)))
             ((pop! (peg:fail-stack))))]
      
      [(any-char)
       #'(if (not (peg:eof?))
             (let ((c (peg:string@)))
               (inc! (peg:sp) 1)
               (sk (char->string c)))
             ((pop! (peg:fail-stack))))]
      
      [(range str)
       #'(if (peg:eof?)
             ((pop! (peg:fail-stack)))
             (let ((c (peg:string@)))
               (if (string-contains-char? str c)
                   (begin
                     (inc! (peg:sp) 1)
                     (sk (char->string c)))
                   ((pop! (peg:fail-stack))))))]
      
      [(string str)
       (let ((str-len (string-length (syntax->datum #'str))))
         #`(if (string-contains-substring? (peg:string) (unbox (peg:sp)) str)
               (begin
                 (inc! (peg:sp) #,str-len)
                 (sk str))
               ((pop! (peg:fail-stack)))))]
      
      [(seq e1 e2)
       #`(let ((mk (lambda (r1)
                    (let ((sk^ (lambda (r2) (sk (peg:join r1 r2)))))
                      #,(peg:compile #'e2 #'sk^)))))
           #,(peg:compile #'e1 #'mk))]
      
      [(choice e1 e2)
       #`(let ((alt (lambda (stash-sp)
                      (lambda ()
                        (set-box! (peg:sp) stash-sp)
                        #,(peg:compile #'e2 #'sk)))))
           (push! (peg:fail-stack) (alt (unbox (peg:sp))))
           #,(peg:compile #'e1 #'sk))]

      [(star e)
       #`(letrec ((k* (lambda (res)
                        (push! (peg:fail-stack) (lambda () (sk (reverse res))))
                        (let ((k-next (lambda (r1)
                                        (k* (cons r1 res)))))
                          #,(peg:compile #'e #'k-next)))))
           (k* '()))]

      [(optional e)
       (peg:compile #'(choice e (epsilon)) #'sk)]
      
      [(plus e)
       #`(let ((sk-cons (lambda (res)
                          (sk (cons (car res)
                                    (cadr res))))))
           #,(peg:compile #'(seq e (star e)) #'sk-cons))]
      
      [(call rule-name)
       (with-syntax ([rule (format-id #'rule-name "peg-rule:~a" #'rule-name)])
         #'(rule sk))]

      [(name nm subexp)
       #`(let ((sk! (lambda (res)
                      (set! nm res)
                      (sk res))))
           #,(peg:compile #'subexp #'sk!))]

      [(not exp)
       ;; if exp fails to parse restore the fail stack and succeed
       ;; if exp succeeds at parsing, restore the fail stack and fail
     ;; TODO: names should not be bound when inside a not
       #`(let ((stash:stack (unbox (peg:fail-stack)))
               (stash:sp (unbox (peg:sp))))
           (set-box! (peg:fail-stack)
                     (list (lambda ()
                             (set-box! (peg:fail-stack) stash:stack)
                             (set-box! (peg:sp) stash:sp)
                             (sk ""))))
           (let ((fk (lambda (_)
                       (set-box! (peg:fail-stack) stash:stack)
                       (set-box! (peg:sp) stash:sp)
                       ((pop! (peg:fail-stack))))))
             #,(peg:compile #'exp #'fk)))]
;      [(not exp) ;; doesnt work
;       ;; if exp fails to parse restore the fail stack and succeed
;       ;; if exp succeeds at parsing, restore the fail stack and fail
;       #`(let ((stash-sp (unbox (peg:sp))))
;           (push! (peg:fail-stack)
;                  (lambda ()
 ;                   (set-box! (peg:sp) stash-sp)
 ;                   (sk "")))
 ;          (let ((fk (lambda (_)
 ;                      (pop! (peg:fail-stack))
 ;                      ((pop! (peg:fail-stack))))))
 ;            #,(peg:compile #'exp #'fk)))]
      
      [else (raise-syntax-error "invalid peg" (syntax->datum exp))])))

(define-syntax (define-peg stx)
  (syntax-case stx (name)
    [(_ rule-name exp) #'(define-peg rule-name (name res exp) res)]
    [(_ rule-name exp action)
     (with-syntax ([name (format-id #'rule-name "peg-rule:~a" #'rule-name)]
                   [body (peg:compile #'exp #'sk^)]
                   [nms (map (lambda (nm) #`(#,nm #f)) (peg:names #'exp))])
       #'(define (name sk)
           (let* nms ;; let* means we don't need to remove duplicates
             (let ((sk^ (lambda (_) (sk action))))
               body))))]))

(define-syntax (peg stx)
  (syntax-case stx ()
    [(_ rule-name str)
     (with-syntax ([name (format-id #'rule-name "peg-rule:~a" #'rule-name)]
                   [err #'(error "parse failed at location" (unbox (peg:sp)))])
       #'(parameterize ([peg:string str]
                        [peg:sp (box 0)]
                        [peg:fail-stack (box (list (lambda () err)))])
           (name (lambda (res)
                   (display "success! ")
                   (write res)
                   (newline)))))]))


;;;;
;; Testing


(define-peg digit
  (range "0123456789"))
(define-peg nonzero-digit
  (range "123456789"))
(define-peg number
  (seq (name hd (call nonzero-digit))
       (name tl (star (call digit))))
  (string->number (string-append* (cons hd tl))))

(define-peg sum
  (seq (name n1 (call number))
       (optional (seq (name op (choice (char #\+) (char #\-)))
                      (name n2 (call sum)))))
  (if op
      `(,op ,n1 ,n2)
      n1))

(define-peg sheep
  (seq (call sh) (seq (call eE2) (string "p!"))))
(define-peg sh (string "sh"))
(define-peg eE
  (choice (seq (string "eE")
               (call eE))
          (epsilon)))
(define-peg eE2
  (plus (choice (string "eE")
                (string "Ee"))))

(define-peg bracks
  (choice (seq (char #\()
               (seq ;(star (char #\x))
                (name res (star (call bracks)))
                (char #\))))
          (name res (call symbl)))
  res)
(define-peg symbl
  (seq (name res (plus (seq (not (char #\())
                            (seq (not (char #\)))
                                 (seq (not (char #\space))
                                      (any-char))))))
       (star (char #\space)))
  (string->symbol (string-append* res)))

(peg sheep "sheEeEeEp!")
(peg sum "3000974+53+4-23424")
(peg bracks "(foo(y()()())bar (baz 429))")
