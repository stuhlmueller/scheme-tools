#!r6rs
(import
  (only (rnrs) let write newline)
  (prefix (only (rnrs) <= - + if) scheme:)
  (only (xitomatl common) time)
  (xitomatl stack-lang))

(define <=
  (λS (x y) (r) (scheme:<= x y)))
(define -
  (λS (x y) (r) (scheme:- x y)))
(define +
  (λS (x y) (r) (scheme:+ x y)))

(define fib
  (Q dup 1 <= (Q drop 1) (Q dup 1 - fib swap 2 - fib +) if))

(define (scheme-fib n)
  (scheme:if (scheme:<= n 1)
    1
    (scheme:+ (scheme-fib (scheme:- n 1))
              (scheme-fib (scheme:- n 2)))))

(time (S 34 fib))
(newline)
(S current-stack print)
(newline)
(let ((v (time (scheme-fib 34))))
  (newline)
  (write v) (newline))
