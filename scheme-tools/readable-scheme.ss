#!r6rs

(library

 (scheme-tools readable-scheme)

 (export rest
         pair
         true?
         false?
         sum
         all
         tagged-list?)

 (import (rnrs))

 (define rest cdr)

 (define pair cons)

 (define (true? obj)
   (eq? obj #t))

 (define (false? obj)
   (eq? obj #f))

 (define (sum vals)
   (apply + vals))

 (define (all proc lst)
   (if (null? lst)
       #t
       (and (proc (car lst))
            (all proc (cdr lst)))))

 (define (tagged-list? obj tag)
   (and (list? obj)
        (not (null? obj))
        (eq? (car obj) tag)))

 )