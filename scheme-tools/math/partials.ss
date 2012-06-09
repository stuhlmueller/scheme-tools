#!r6rs

(library

 (scheme-tools math partials)

 (export partials)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (scheme-tools math AD))

 (define (f-n f n)
   (lambda (vars)
     (vector-ref (f vars) n)))

 (define (partials f y-dim)
   (let ([Dfs (vector-map (lambda (i) (gradient-vector-R (f-n f i)))
                          (list->vector (iota y-dim)))])
     (lambda (vars)
       (vector-map (lambda (Df) (Df vars))
                   Dfs))))

 )

