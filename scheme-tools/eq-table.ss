#!r6rs

(library

 (scheme-tools eq-table)

 (export make-eq-table
         eq-table-add!
         eq-table-lookup)

 (import (rnrs)
         (rnrs mutable-pairs)
         (scheme-tools readable-scheme))

 (define (make-eq-table)
   (pair '() #f))

 (define (eq-table-add! table-container key val)
   (set-car! table-container
             (cons (cons key val) (car table-container))))

 (define (eq-table-lookup table-container key default-lambda)
   (let ([val (assq key (car table-container))])
     (if val
         (cdr val)
         (default-lambda))))

 )