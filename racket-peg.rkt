#lang racket

;; char c 	If the character SP points at is not c, stop this
;;              thread: it failed. Otherwise, advance SP to the next
;;              character and advance PC to the next instruction.
;;
;; match 	Stop this thread: it found a match.
;;
;; jmp x 	Jump to (set the PC to point at) the instruction at x.
;;
;; split x, y 	Split execution: continue at both x and y. Create a
;;              new thread with SP copied from the current thread. One
;;              thread continues with PC x. The other continues with PC
;;              y. (Like a simultaneous jump to both locations.)

(struct ctx (str [sp #:mutable]))
(define vm-ctx (make-parameter #f))

(define (vm-ctx:@sp)
  (let ((cx (vm-ctx)))
    (string-ref (ctx-str cx) (ctx-sp cx))))
(define (vm-ctx:sp++)
  (let ((cx (vm-ctx)))
    (set-ctx-sp! cx (+ (ctx-sp cx) 1))))
(define (vm-ctx:push! )
  (let ((cx (vm-ctx)))
    )))


(define-struct (match-fail exn:fail:user) ())
(define-syntax (vm:fail stx)
  (syntax-case stx ()
    [(_)
     #`(raise (make-match-fail "unknown reason" (current-continuation-marks)))]))

(define-syntax (vm:char stx)
  (syntax-case stx ()
    [(_ chr)
     #`(if (char=? chr (vm-ctx:@sp))
           (vm-ctx:sp++)
           (vm:fail))]))
(define-syntax (vm:split stx)
  (syntax-case stx ()
    [(_ lbl-1 lbl-2)
     #`()]))

(define-syntax (re:compile stx)
  (syntax-case stx (epsilon seq choice
                            lbl-1 lbl-2 lbl-3)
    [(_ (epsilon))
     #'(begin/goto
         #t)]
    [(_ (seq e1 e2))
     #'(begin/goto
         (re:compile e1)
         (re:compile e2))]
    [(_ (seq e1 e2 e3 ...))
     #'(begin/goto
         (re:compile e1)
         (re:compile e2) 
         (re:compile e3) ...)]
    [(_ (choice e1 e2))
     #'(begin/goto
         (vm:split lbl-1 lbl-2)
         (label lbl-1)
         (re:compile e1)
         (goto lbl-3)
         (label lbl-2)
         (re:compile e2)
         (label lbl-3))]
    [(_ ch)
     (let ((ch^ (syntax->datum #'ch)))
       (cond [(char? ch^)
              #'(vm:char ch)]
             [(string? ch^)
              (let ((string^ (foldr (lambda (x ys) #`(seq #,x #,ys))
                                    #'(epsilon)
                                    (string->list ch^))))
                #`(re:compile #,string^))]
             [else
              (raise-syntax-error 're "expected a character" ch^)]))]))

(define (test-re str body)
  (with-handlers ([match-fail? (lambda (e)
                                 #f)])
    (parameterize ([vm-ctx (ctx str 0)])
      (body))))

(define t1 (test-re "foobar"
                    (lambda ()
                      (re:compile (seq #\a #\b)))))
(define t1b (test-re "abba"
                    (lambda ()
                      (re:compile (seq #\a #\b)))))
(define t2 (test-re "foobar"
                    (lambda ()
                      (re:compile (seq #\f "oobar")))))
(define t2b (test-re "foobar"
                     (lambda ()
                      (re:compile (seq #\f "oobaz")))))
(define t2c (test-re "foobar"
                     (lambda ()
                       (re:compile (seq #\f "ooba" (choice #\z #\r))))))
