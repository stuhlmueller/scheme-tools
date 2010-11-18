#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (xitomatl vectors)
  (srfi :78 lightweight-testing))

(define-syntax check-invalid
  (syntax-rules ()
    ((_ expr)
     (check (guard (ex (else (and (assertion-violation? ex)
                                  (who-condition? ex)
                                  (message-condition? ex)
                                  (list (condition-who ex)
                                        (condition-message ex)))))
              expr
              'unexpected-return)
            => '(subvector "invalid indices")))))

(check (subvector '#() 0 0) => '#())
(check-invalid (subvector '#() 0 1))
(check-invalid (subvector '#() 1 0))
(check-invalid (subvector '#() 1 1))
(check (subvector '#(1) 0 0) => '#())
(check (subvector '#(a) 0 1) => '#(a))
(check (subvector '#(a) 1 1) => '#())
(check-invalid (subvector '#(1) 0 2))
(check-invalid (subvector '#(1) 1 0))
(check-invalid (subvector '#(1) 55 55))
(check (subvector '#(a b) 0 0) => '#())
(check (subvector '#(a b) 0 1) => '#(a))
(check (subvector '#(a b) 1 2) => '#(b))
(check (subvector '#(a b) 0 2) => '#(a b))
(check (subvector '#(a b) 2 2) => '#())
(check-invalid (subvector '#(a b) 3 3))
(check-invalid (subvector '#(a b) 1 0))
(check-invalid (subvector '#(a b) 2 3))
(check (subvector '#(a b c d e f) 0 0) => '#())
(check (subvector '#(a b c d e f) 0 1) => '#(a))
(check (subvector '#(a b c d e f) 1 2) => '#(b))
(check (subvector '#(a b c d e f) 0 2) => '#(a b))
(check (subvector '#(a b c d e f) 2 2) => '#())
(check (subvector '#(a b c d e f) 0 5) => '#(a b c d e))
(check (subvector '#(a b c d e f) 1 5) => '#(b c d e))
(check (subvector '#(a b c d e f) 4 5) => '#(e))
(check (subvector '#(a b c d e f) 5 6) => '#(f))
(check (subvector '#(a b c d e f) 3 6) => '#(d e f))
(check (subvector '#(a b c d e f) 2 4) => '#(c d))
(check (subvector '#(a b c d e f) 6 6) => '#())
(check-invalid (subvector '#(a b c d e f) 5 4))
(check-invalid (subvector '#(a b c d e f) 1 7))
(check-invalid (subvector '#(a b c d e f) 42 45))


(check-report)
