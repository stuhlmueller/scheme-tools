#!r6rs

;; Returns false the first time it observes an object, true for every
;; subsequent observation of the same object.

(library

 (scheme-tools watcher)

 (export make-watcher)

 (import (rnrs)
         (scheme-tools hash)
         (scheme-tools external))

 (define (make-watcher . hash-table-maker)
   (let ([watch-table (if (null? hash-table-maker)
                          (make-finitize-hash-table)
                          ((car hash-table-maker)))])
     (lambda (obj)
       (hash-table-ref watch-table
                       obj
                       (lambda () (begin
                               (hash-table-set! watch-table obj #t)
                               #f))))))

 )