#!r6rs

(library

 (scheme-tools table)

 (export make-table
         table-add!
         table-lookup
         table-lookup/set!)

 (import (rnrs)
         (rnrs mutable-pairs)
         (scheme-tools readable-scheme))

 (define (make-table equality)
   (pair '() equality))

 (define (table-add! table-container key val)
   (set-car! table-container
             (cons (cons key val) (car table-container))))

 (define (table-lookup table-container key default-lambda)
   (let* ([equality (cdr table-container)]
          [val (find (lambda (v) (equality (car v) key)) (car table-container))])
     (if val
         (cdr val)
         (default-lambda))))

 (define (table-lookup/set! table-container key default-lambda)
   (table-lookup table-container
                 key
                 (lambda () (let ([val (default-lambda)])
                         (table-add! table-container key val)
                         val))))

 )