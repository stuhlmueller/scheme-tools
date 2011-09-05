#!r6rs

(library

 (scheme-tools lists)

 (export all-combinations
         map-enumerate
         union)

 (import (rnrs)
         (scheme-tools readable-scheme)
         (scheme-tools srfi-compat :1))

 ;; e.g. [(1 2) (a b)] => [(1 a) (1 b) (2 a) (2 b)]
 (define (all-combinations lsts)
   (let loop ([lsts lsts]
              [acc '(())])
     (if (null? lsts)
         (map reverse acc)
         (loop (cdr lsts)
               (apply append
                      (map (lambda (fst)
                             (map (lambda (s) (cons fst s))
                                  acc))
                           (car lsts)))))))

 (define (map-enumerate proc . lsts)
   (apply map
          (pair proc
                (pair (iota (length (first lsts)))
                      lsts))))

 (define (union lsts equality)
   (delete-duplicates (apply lset-union (cons equality lsts))))
 
 )