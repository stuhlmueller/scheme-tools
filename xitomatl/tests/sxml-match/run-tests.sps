#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (xitomatl sxml-match)
  (xitomatl include)
  (srfi :78 lightweight-testing)
  (only (xitomatl common) printf))

  (define-syntax module
    (syntax-rules ()
      ((_ _ _ . r) (begin . r))))
  
  (define-syntax require
    (syntax-rules ()
      ((_ . _) (begin))))
  
  (define-syntax run-test
    (syntax-rules ()
      ((_ desc test expected-result)
       (begin
         (printf "\nRunning: ~a:\n" desc)
         (check test => expected-result)))))

(include/resolve ("xitomatl" "tests" "sxml-match") "sxml-match-tests.ss")

(check-report)
