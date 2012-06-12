#!r6rs

(library

 (scheme-tools hashtable)

 (export hashtable->alist
         hashtable-ref/default
         hashtable-ref/nodef
         hashtable-for-each
         hashtable-keys
         hashtable-values
         pretty-print-hashtable
         hashtable-set!/assert-consistent)

 (import (rnrs)
         (scheme-tools srfi-compat :43)
         (only (scheme-tools readable-scheme) pe)
         (only (scheme-tools external) pretty-print gensym))

 (define not-found (gensym 'not-found))

 (define (hashtable->alist table)
   (let-values ([(keys vals) (hashtable-entries table)])
     (map cons
          (vector->list keys)
          (vector->list vals))))

 (define (hashtable-for-each proc table)
   (let-values ([(keys vals) (hashtable-entries table)])
     (vector-for-each proc
                      keys
                      vals)))

 (define (hashtable-ref/nodef table key)
   (let ([v (hashtable-ref table key not-found)])
     (if (eq? v not-found)
         (error key "not found")
         v)))

 (define (hashtable-ref/default table key thunk)
   (let ([v (hashtable-ref table key not-found)])
     (if (eq? v not-found)
         (thunk)
         v)))

 (define (hashtable-values ht)
   (let-values ([(keys vals) (hashtable-entries ht)])
     vals))

 (define (pretty-print-hashtable ht)
   (let-values ([(keys vals) (hashtable-entries ht)])
     (for-each (lambda (k v) (pretty-print (cons k v)))
               (vector->list keys)
               (vector->list vals))))

 (define (hashtable-set!/assert-consistent table key value)
   (let* ([hash-table-miss (gensym)]
          [existing-value (hashtable-ref table key hash-table-miss)])
     (if (eq? existing-value hash-table-miss)
         (hashtable-set! table key value)
         (when (not (equal? existing-value value))
               (begin (pe " " key " is bound to " existing-value ", can't set to " value "\n")
                      (error #f "hashtable-set!/assert-unbound: not unbound"))))))

 )