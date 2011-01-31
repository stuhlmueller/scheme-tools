(library

 ;; WARNING:
 ;; This identifies all procedures for the purpose of hashing. We
 ;; assume that any object given fully mirrors procedure information in
 ;; an accessible way.

 (scheme-tools mem)

 (export mem)

 (import (rnrs)
         (only (srfi :1) first second)
         (scheme-tools readable-scheme)
         (scheme-tools hash)
         (scheme-tools table)
         (scheme-tools))
 
 (define RECURSIVE-MEM-VALUE 'calling)

 (define memtables (make-table eq?))

 (define (get/make-memtable f)
   (table-lookup/set! memtables f make-finitize-hash-table))
 
 (define (mem f)
   (lambda args
     (let ([mt (get/make-memtable f)])
       (hash-table-ref mt
                       args
                       (lambda () (begin
                               (when RECURSIVE-MEM-VALUE
                                     (hash-table-set! mt args RECURSIVE-MEM-VALUE))
                               (let ([val (apply f args)])
                                 (hash-table-set! mt args val)
                                 val)))))))

 )