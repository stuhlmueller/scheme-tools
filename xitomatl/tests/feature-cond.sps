#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (rnrs eval)
  (srfi :78 lightweight-testing)
  (only (xitomatl exceptions) catch)
  (xitomatl feature-cond))

(define-syntax check-ex/no-clause-true
  (syntax-rules ()
    ((_ expr)
     (check (catch ex
                ((else (and (assertion-violation? ex)
                            (who-condition? ex)
                            (eq? (quote feature-cond) (condition-who ex))
                            (message-condition? ex)
                            (string=? "no clause true" (condition-message ex)))))
              expr
              (quote unexpected-return))
            => #T))))

(define-syntax check-ex/invalid-test-syntax
  (syntax-rules ()
    ((_ expr)
     (check (catch ex
                ((else (and (syntax-violation? ex)
                            (who-condition? ex)
                            (eq? (quote feature-cond) (condition-who ex))
                            (message-condition? ex)
                            (string=? "invalid feature syntax"
                                      (condition-message ex)))))
              (eval (quote expr)
                    (environment (quote (xitomatl feature-cond))))
              (quote unexpected-return))
            => #T))))

(check-ex/no-clause-true (feature-cond))
(check-ex/no-clause-true (feature-cond (some-feature-which-does-not-exist #F)))

(check (feature-cond (else (quote else))) => (quote else))
(check (feature-cond (some-feature-which-does-not-exist #F)
                     (else (quote else)))
       => (quote else))
(check (feature-cond (r6rs)) => #T)
(check (feature-cond (r6rs (quote ok)) (else #F)) => (quote ok))

(check-ex/invalid-test-syntax (feature-cond ((and blah or) #F)))
(check-ex/invalid-test-syntax (feature-cond ((or blah and) #F)))
(check-ex/invalid-test-syntax (feature-cond ((not) #F)))
(check-ex/invalid-test-syntax (feature-cond ((not blah blah) #F)))
(check-ex/invalid-test-syntax (feature-cond ((not else) #F)))
(check-ex/invalid-test-syntax (feature-cond ((not not) #F)))

(check (feature-cond ((and r6rs (and) (srfi :1))
                      (quote ok))
                     (else #F))
       => (quote ok))
(check (feature-cond ((and r6rs some-feature-which-does-not-exist)
                      (quote first))
                     ((and r6rs (and srfi-1 (and some-feature-which-does-not-exist)))
                      (quote second))
                     (else
                      (quote ok)))
       => (quote ok))
(check (feature-cond ((or some-feature-which-does-not-exist
                          (or)
                          srfi-1)
                      (quote ok))
                     (else #F))
       => (quote ok))
(check (feature-cond ((or) (quote first))
                     ((or (or (or some-feature-which-does-not-exist)))
                      (quote second))
                     ((or r6rs) (quote ok)))
       => (quote ok))
(check (feature-cond ((not some-feature-which-does-not-exist)
                      (quote ok))
                     (else #F))
       => (quote ok))
(check (feature-cond ((not r6rs)
                      (quote first))
                     ((not (not (not some-feature-which-does-not-exist)))
                      (quote ok))
                     (else #F))
       => (quote ok))
(check (feature-cond ((or (and r6rs SFWDNE) (not (and)))
                      (quote first))
                     ((not (and (not (or)) (or SFWDNE (srfi :123456789))))
                      (quote ok))
                     (else #F))
       => (quote ok))
(check-ex/no-clause-true
 (feature-cond ((not (and (not (or)) (or SFWDNE (not (srfi :123456789)))))
                #F)))

(let ((and 1) (or 2) (not 3) (else 4))
  (check (feature-cond ((and) #T)) => #T)
  (check (feature-cond ((or r6rs) #T)) => #T)
  (check (feature-cond ((not SFWDNE) #T)) => #T)
  (check (feature-cond (else #T)) => #T))

(check-report)
