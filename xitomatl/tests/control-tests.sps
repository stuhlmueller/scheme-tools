#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (rnrs eval)
  (srfi :78 lightweight-testing)
  (only (xitomatl exceptions) catch)
  (xitomatl control))

(define-syntax check-values
  (syntax-rules (=>)
    ((_ expr => vals ...)
     (check (let-values ((v expr)) v) => (list vals ...)))))

(define-syntax check-SV
  (syntax-rules ()
    ((_ expr)
     (check (catch ex ((else (syntax-violation? ex)))
              (eval 'expr (environment '(rnrs) '(xitomatl control)))
              'unexpected-return)
            => #T))))

;;;; begin0

(let ((x 1) (n 0))
  (check (begin0 
           (begin (set! n (+ 1 n))
                  (- x))
           (set! x (+ 1 x))
           (set! x (* x x))) 
         => -1)
  (check x => 4)
  (check n => 1))
(check-SV (begin0))

;;;; compose

(define (add1 x) (+ 1 x))

(check-values ((compose))
              => )
(check-values ((compose) 1)
              => 1)
(check-values ((compose) 1 2 3)
              => 1 2 3)
(check-values ((compose -) 2)
              => -2)
(check-values ((compose -) 9 8 7)
              => -6)
(check-values ((compose / - add1) 2)
              => -1/3)
(check-values ((compose add1 - /) 2)
              => 1/2)
(check-values ((compose (lambda args (apply values (cddr (map - args))))
                        (lambda args (apply values (reverse args))))
               1 2 3 4 5)
              => -3 -2 -1)
(check-values ((compose string->number
                        list->string
                        vector->list
                        vector
                        (lambda () (values #\1 #\2))))
              => 12)


(check-report)
