#!r6rs

;; WARNING:
;; This identifies all procedures that occur in the arguments of a
;; memoized function for the purpose of hashing. We assume that any
;; object given fully mirrors procedure information in an accessible
;; way.

(library

 (scheme-tools mem)

 (export mem
         recursive-mem)

 (import (rnrs)
         (only (srfi :1) first second)
         (scheme-tools readable-scheme)
         (scheme-tools hash)
         (scheme-tools table)
         (scheme-tools))

 (define (mem f)
   (let ([memtable (make-finitize-hash-table)])
     (lambda args
       (hash-table-ref
        memtable
        args
        (lambda () (let ([val (apply f args)])
                     (hash-table-set! memtable args val)
                     val))))))

 (define (recursive-mem f make-recursion-value)
   (let ([memtable (make-finitize-hash-table)])
     (lambda args
       (hash-table-ref
        memtable
        args
        (lambda () (begin
                     (hash-table-set! memtable args (make-recursion-value))
                     (let ([val (apply f args)])
                       (hash-table-set! memtable args val)
                       val)))))))

 )