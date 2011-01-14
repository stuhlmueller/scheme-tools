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
         (scheme-tools hash))
 
 ;; look up value for key in alist; if not found,
 ;; set (default-thunk) as value and return it
 (define (get/make-alist-entry alist alist-set! key default-thunk)
   (let ([binding (assq key alist)])
     (if binding
         (rest binding)
         (let* ([default-val (default-thunk)])
           (alist-set! key default-val)
           default-val))))

 (define memtables '())

 (define (get/make-memtable f)
   (get/make-alist-entry memtables
                         (lambda (k v) (set! memtables (pair (pair k v) memtables)))
                         f
                         (lambda () (make-hash-table))))

 (define (mem f)
   (lambda args
     (let ([mt (get/make-memtable f)])
       (hash-table-ref mt
                       args
                       (lambda () (let ([val (apply f args)])
                               (hash-table-set! mt args val)
                               val))))))

 )