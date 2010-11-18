#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import 
  (rnrs)
  (only (xitomatl exceptions) catch)
  (xitomatl curry)
  (srfi :78 lightweight-testing))

(define-syntax check-values
  (syntax-rules ()
    ((_ expr => vals ...)
     (check (let-values ((v expr)) v) => (list vals ...)))))

(define-syntax check-AV
  (syntax-rules ()
    ((_ expr)
     (check (catch ex ((else (assertion-violation? ex)))
              expr
              'unexpected-return)
            => #T))))

(define list-4-2
  ((curry list 3) 4 2))
(check (list-4-2 5) => '(4 2 5))
(check (list-4-2 5 6) => '(4 2 5 6))
(check ((((curry vector 3) 2) 4) 5) => '#(2 4 5))
(check ((curry vector 0)) => '#())
(check ((curry vector 0) 1 2) => '#(1 2))
(check-AV (curry vector -1))
(check-AV (curry vector "oops"))

(check ((lambda/curry () 'ok)) => 'ok)
(check ((lambda/curry r r)) => '())
(check ((lambda/curry r (reverse r)) 1 2 3) => '(3 2 1))

(define/curry (c1 x) (- x))
(check (c1 42) => -42)
(check ((((c1))) 33) => -33)

(define/curry (c2 x y) (- y x))
(define c2a (c2 11))
(check (c2a 1) => -10)
(check (c2a 20) => 9)
(define c2b (c2 7))
(check (c2b 1) => -6)
(check (c2b 20) => 13)
(check (c2 55 23) => -32)

(define/curry (c3 x y z . r)
  (values r z y x))
(check-values (c3 1 2 3 4 5 6)
              => '(4 5 6) 3 2 1)
(define c3a (c3 1 2))
(check-values (c3a 3) => '() 3 2 1)
(check-values (c3a 3 4 5 6 7) => '(4 5 6 7) 3 2 1)
(define c3b (c3 1))
(check-values ((c3b 2) 3) => '() 3 2 1)
(check-values (c3b 2 3) => '() 3 2 1)
(check-values ((c3b 2) 3 4 5) => '(4 5) 3 2 1)

(define/curry (c4 a b c d e f g) (vector a b c d e f g))
(check ((((((((c4) 1) 2) 3) 4) 5) 6) 7) => '#(1 2 3 4 5 6 7))
(check (((((c4 1 2) 3 4) 5) 6) 7) => '#(1 2 3 4 5 6 7))
(check ((((c4 1 2) 3 4 5) 6) 7) => '#(1 2 3 4 5 6 7))
(check ((((c4 1) 2) 3 4 5 6) 7) => '#(1 2 3 4 5 6 7))
(check (c4 1 2 3 4 5 6 7) => '#(1 2 3 4 5 6 7))
(check ((c4 1 2 3 4) 5 6 7) => '#(1 2 3 4 5 6 7))

(check-report)
