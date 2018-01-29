#lang racket

;; based on https://swtch.com/~rsc/regexp/regexp2.html

(define-syntax-rule (pop! place)
  (let ((top (car place)))
    (set! place (cdr place))
    top))

(define-syntax-rule (push! thing place)
  (set! place (cons thing place)))

(define-for-syntax (re:compile str fail alt exp lbl end)
  (with-syntax ([str str] [fail fail] [alt alt] [lbl lbl] [end end])
    (syntax-case exp (char seq choice star)
        
      [(char c)
       (list #'(lbl (lambda (sp)
                      (if (char=? c (string-ref str sp))
                          (end (+ sp 1))
                          (fail)))))]
        
      [(seq e1 e2)
       (with-syntax ([(mid) (generate-temporaries #'(mid))])
         (append (re:compile #'str #'fail #'alt #'e1 #'lbl #'mid)
                 (re:compile #'str #'fail #'alt #'e2 #'mid #'end)))]
        
      [(choice e1 e2)
       (with-syntax ([(lbl-1 lbl-2) (generate-temporaries #'(e1 e2))])
         (append (list #'(lbl (lambda (sp)
                                (alt (lambda () (lbl-2 sp)))
                                (lbl-1 sp))))
                 (re:compile #'str #'fail #'alt #'e1 #'lbl-1 #'end)
                 (re:compile #'str #'fail #'alt #'e2 #'lbl-2 #'end)))]
        
      [(star e1)
       (with-syntax ([(mid) (generate-temporaries #'(mid))])
         (append (list #'(lbl (lambda (sp)
hah                                (alt (lambda () (end sp)))
                                (mid sp))))
                 (re:compile #'str #'fail #'alt #'e1 #'mid #'lbl)))]
        
      [else
       (raise-syntax-error "invalid re" (syntax->datum exp))])))
  
(define-syntax (re stx)
  (syntax-case stx ()
    [(_ exp)
     (with-syntax ([(binding ...) (re:compile #'str #'fail #'alt #'exp #'start #'end)])
       #'(lambda (str)
           (let ((alternatives (list (lambda () #f))))
             (letrec ((fail (lambda () ((pop! alternatives))))
                      (alt (lambda (thunk) (push! thunk alternatives)))
                      (end (lambda (sp)
                             (display 'done)
                             (display sp)
                             (newline)))
                      binding ...)
               (start 0)))))]))

(define t1 (re (char #\a)))
(define t2 (re (seq (char #\a) (char #\b))))
(define t3 (re (seq (char #\a) (seq (char #\b) (char #\c)))))
(define t4 (re (seq (char #\a) (seq (choice (char #\b) (seq (char #\x) (char #\y))) (char #\c)))))
(define t5 (re (seq (char #\s) (seq (char #\h) (seq (star (seq (char #\e) (char #\E))) (char #\p))))))
