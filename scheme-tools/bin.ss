#!r6rs

(library

 (scheme-tools bin)

 (export bin)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (scheme-tools readable-scheme)
         (scheme-tools hash))

 (define (bin vals)
   (let ([table (make-equal-hash-table)])
     (hash-bin vals table)))

 (define (hash-bin vals table)
   (if (null? vals)
       (hash-table->alist table)
       (let ([count (hash-table-ref/default table (first vals) 0)])
         (hash-table-set! table (first vals) (+ count 1))
         (hash-bin (rest vals) table))))

 )