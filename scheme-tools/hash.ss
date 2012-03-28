#!r6rs

;; equal?-based hash-table that can deal with recursive references

;; finitize-hash-table identifies all procedures for the purpose of
;; hashing (but not for equality).

(library

 (scheme-tools hash)

 (export (rename (default-make-hash-table make-hash-table))
         make-finitize-hash-table
         make-equal-hash-table
         make-eq-hash-table
         alist->hash-table
         hash-table?
         hash-table->alist
         hash-table-ref
         hash-table-ref/default
         hash-table-set!
         hash-table-delete!
         hash-table-exists?
         hash-table-update!
         hash-table-update!/default
         hash-table-size
         hash-table-keys
         hash-table-values
         hash-table-walk
         hash-table-fold
         hash-table-copy
         hash-table-merge!
         finitize
         finitize-equal?
         finitize-hash)

 (import (rnrs)
         (scheme-tools readable-scheme)
         (scheme-tools external)
         (scheme-tools srfi-compat :1)
         (scheme-tools srfi-compat :69))

 (define/kw (finitize obj
                      [handler :default (lambda (x) x)]
                      [keep-procs :default #f])
   (define seen (make-eq-hash-table))
   (define sym (get-counter))
   (define (fin obj)
     (hash-table-ref seen
                     obj
                     (lambda ()
                       (cond [(procedure? obj) (if keep-procs obj 'proc)]
                             [(pair? obj)
                              (begin (hash-table-set! seen obj (sym))
                                     (cons (fin (car obj))
                                           (fin (cdr obj))))]
                             [(vector? obj)
                              (begin (hash-table-set! seen obj (sym))
                                     (map fin (vector->list obj)))]
                             [else (handler obj)]))))
   (fin obj))

 (define *default-bound* (- (expt 2 29) 3))

 (define (equality-hash obj . maybe-bound)
   (inexact->exact
    (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
      (cond ((integer? obj) (modulo obj bound))
            ((string? obj) (string-hash obj))
            ((symbol? obj) (symbol-hash obj))
            ((and (real? obj) (= -inf.0 obj)) (string-hash "-inf.0"))
            ((and (real? obj) (= +inf.0 obj)) (string-hash "+inf.0"))
            ((and (real? obj) (= +nan.0 obj)) (string-hash "+nan.0"))
            ((real? obj) (inexact->exact (modulo (+ (numerator obj) (denominator obj)) bound)))
            ((number? obj)
             (modulo (+ (equality-hash (real-part obj)) (* 3 (equality-hash (imag-part obj))))
                     bound))
            ((char? obj) (modulo (char->integer obj) bound))
            ((vector? obj) (vector-hash obj bound))
            ((pair? obj) (modulo (+ (equality-hash (car obj)) (* 3 (equality-hash (cdr obj))))
                                 bound))
            ((null? obj) 0)
            ((not obj) 0)
            ((procedure? obj) (error obj "equality-hash: procedures cannot be hashed"))
            (else 1)))))

 (define (vector-hash v bound)
   (let ((hashvalue 571)
         (len (vector-length v)))
     (do ((index 0 (+ index 1)))
         ((>= index len) (modulo hashvalue bound))
       (set! hashvalue (modulo (+ (* 257 hashvalue) (equality-hash (vector-ref v index)))
                               *default-bound*)))))

 (define/kw (finitize-hash obj [handler :default (lambda (x) x)] [keep-procs :default #f] . bound)
   (equality-hash (finitize obj 'handler handler 'keep-procs keep-procs)
                  (if (null? bound) *default-bound* bound)))

 (define (finitize-equal? obj1 obj2)
   (equal? obj1 obj2))

 (define (default-make-hash-table . args)
   (when (not (= (length args) 2))
         (error args "make-hash-table: hash and equality need to be provided."))
   (apply make-hash-table args))

 (define (make-eq-hash-table)
   (make-hash-table eq?))

 (define (make-finitize-hash-table)
   (make-hash-table finitize-equal? finitize-hash))

 (define (make-equal-hash-table)
   (make-hash-table equal? (lambda args (inexact->exact (apply equality-hash args)))))

 (define (test)
   (define test-obj (vector (lambda (x) x) 2 3))
   (define ht (make-hash-table))
   (vector-set! test-obj 2 test-obj)
   (hash-table-set! ht test-obj 1)
   (display (hash-table-ref ht test-obj)))

 )