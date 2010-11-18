#!r6rs
;; Copyright 2010 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl tests irregex test)
  (export
    use load
    test test-assert test-error test-group
    test-begin test-end test-exit)
  (import
    (rnrs)
    (srfi :78 lightweight-testing))

  (define-syntax use (syntax-rules () ((_ . _) (begin))))
  (define-syntax load (syntax-rules () ((_ . _) (begin))))

  (define-syntax test
    (syntax-rules ()
      ((_ name expected expr)
       (test expected expr))
      ((_ expected expr)
       (check expr => expected))))

  (define-syntax test-assert
    (syntax-rules ()
      ((_ name expr)
       (test-assert expr))
      ((_ expr)
       (check (and expr #T) => #T))))

  (define-syntax test-error
    (syntax-rules ()
      ((_ name expr)
       (test-error expr))
      ((_ expr)
       (check (guard (ex ((or (error? ex)
                              (assertion-violation? ex))
                          #T)
                         (else `(dont-know: ,ex)))
                expr
                '(succeeded: expr))
              => #T))))

  (define-syntax test-group
    (syntax-rules ()
      ((_ name expr0 expr ...)
       (begin expr0 expr ...))))

  (define-syntax test-begin
    (syntax-rules ()
      ((_)
       (begin))))

  (define (test-end) (check-report))

  (define (test-exit n)
    (if (check-passed? n)
      (exit)
      (exit #F)))

)
