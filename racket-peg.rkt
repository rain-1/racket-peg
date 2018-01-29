#lang racket

(require (for-syntax racket/syntax))
(require (for-syntax racket/match))

(define-syntax-rule (pop! place)
  (let ((top (car (unbox place))))
    (set-box! place (cdr (unbox place)))
    top))

(define-syntax-rule (push! place thing)
  (set-box! place (cons thing (unbox place))))

(define peg:string (make-parameter #f))
(define peg:sp (make-parameter #f))
(define peg:fail-stack (make-parameter #f))
(define (peg:eof?) (= (unbox (peg:sp)) (- (string-length (peg:string)) 0)))

(define (peg:string@)
  (string-ref (peg:string) (unbox (peg:sp))))

(define (inc! b n)
  (set-box! b (+ (unbox b) n)))

(define (string-contains-substring? str place contained)
  (and (>= (string-length str) (+ place (string-length contained)))
       (string=? (substring str place (+ place (string-length contained)))
                 contained)))

(define (char->string ch) (list->string (list ch)))

(define (string-contains-char? str ch)
  (string-contains? str (char->string ch)))

(define (peg:join x y)
  (cond ((and (string? x) (string? y)) (string-append x y))
        ((and (list? x) (list? y)) (append x y))
        (else (list x y))))

(define-for-syntax (peg:names exp)
  (syntax-case exp (epsilon char string range seq choice star optional plus call name)
    [(seq e1 e2) (append (peg:names #'e1) (peg:names #'e2))]
    [(choice e1 e2) (append (peg:names #'e1) (peg:names #'e2))]
    [(star e1) (peg:names #'e1)]
    [(optional e1) (peg:names #'e1)]
    [(plus e1) (peg:names #'e1)]
    
    [(name nm subexp) (cons #'nm (peg:names #'subexp))]

    [else '()]))

(define-for-syntax (peg:compile exp sk)
  (with-syntax ([sk sk])
    (syntax-case exp (epsilon char string range seq choice star optional plus call name)

      [(epsilon)
       #'(sk "")]
      
      [(char c)
       #'(if (and (not (peg:eof?)) (char=? c (peg:string@)))
             (begin
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
       #`(let ((k1 (lambda () #,(peg:compile #'e1 #'sk)))
               (k2 (lambda () #,(peg:compile #'e2 #'sk))))
           (push! (peg:fail-stack) k2)
           (k1))]

      [(star e)
       #`(letrec ((k* (lambda (res)
                        (push! (peg:fail-stack) (lambda () (sk res)))
                        (let ((k-next (lambda (r1)
                                        (k* (peg:join res r1)))))
                          #,(peg:compile #'e #'k-next)))))
           (k* ""))]

      [(optional e)
       (peg:compile #'(choice e (epsilon)) #'sk)]
      
      [(plus e)
       (peg:compile #'(seq e (star e)) #'sk)]
      
      [(call rule-name)
       (with-syntax ([rule (format-id #'rule-name "peg-rule:~a" #'rule-name)])
         #'(rule sk))]

      [(name nm subexp)
       #`(let ((sk! (lambda (res)
                      (set! nm res)
                      (sk res))))
           #,(peg:compile #'subexp #'sk!))]
      
      [else (raise-syntax-error "invalid peg" (syntax->datum exp))])))

(define-syntax (define-peg stx)
  (syntax-case stx (name)
    [(_ rule-name exp)
     #'(define-peg rule-name (name res exp) res)]
    [(_ rule-name exp action)
     (with-syntax ([name (format-id #'rule-name "peg-rule:~a" #'rule-name)]
                   [body (peg:compile #'exp #'sk^)]
                   [nms (map (lambda (nm) #`(#,nm #f)) (peg:names #'exp))])
       #'(define (name sk)
           (let nms
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
                   (display res)
                   (newline)))))]))

(define-peg digit
  (range "0123456789"))
(define-peg nonzero-digit
  (range "123456789"))
(define-peg number
  (name res (seq (call nonzero-digit) (star (call digit))))
  (string->number res))

(define-peg sum
  (seq (name n1 (call number))
       (optional (seq (name op (choice (char #\+) (char #\-)))
                      (name n2 (call sum)))))
  (if op
      `(,op ,n1 ,n2)
      n1))

(define-peg sheep
  (seq (call sh) (seq (call eE2) (char #\p))))
(define-peg sh (string "sh"))
(define-peg eE
  (choice (seq (string "eE")
               (call eE))
          (epsilon)))
(define-peg eE2
  (plus (choice (string "eE")
                (string "Ee"))))

(peg sheep "sheEeEeEp")
(peg sum "3000974+53+4-23424")

