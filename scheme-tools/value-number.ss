#!r6rs

;; FIXME: make sure probability of hash collisions is low (all primitive procedures in one bucket)
;; FIXME: Make &expand-recursive more robust (don't rely on &... symbols not being used elsewhere)
;;
;; All functions that start with & operate on value numbers (input and
;; output). Exceptions are marked. For example, &vector?->b returns a
;; Boolean, and &list-ref/n takes the list index as a number.

(library

 (scheme-tools value-number)

 (export &*
         &+
         &-
         &/
         &<
         &<=
         &=
         &>
         &>=
         &and
         &cadddr
         &caddr
         &cadr
         &car
         &cddddr
         &cdddr
         &cddr
         &cdr
         &cons
         &eq?
         &eq?->b
         &expand-boolean
         &expand-list
         &expand-null
         &expand-number
         &expand-pair
         &expand-procedure
         &expand-recursive
         &expand-step
         &expand-symbol
         &expand-vector
         &false
         &id
         &list
         &list-ref
         &list-ref/n
         &not
         &null
         &null?
         &null?->b
         &or
         &pair?
         &pair?->b
         &reverse
         &symbol?
         &symbol?->b
         &tagged-list?->b
         &true
         &vector
         &vector-append
         &vector-index
         &vector-length
         &vector-ref
         &vector?
         &vector?->b
         compress-boolean
         compress-list
         compress-null
         compress-number
         compress-pair
         compress-procedure
         compress-recursive
         compress-symbol
         compress-vector
         make-number-store
         make-obj-store
         number-store
         obj-store
         value-number?)

 (import (rnrs)
         (scheme-tools srfi-compat :43)
         (scheme-tools srfi-compat :1)
         (scheme-tools hashtable)
         (scheme-tools debug)
         (scheme-tools implementation-specific)
         (scheme-tools readable-scheme))

 (define readable-gensym (symbol-maker '&))

 (define not-found (gensym 'not-found))

 (define (not-found? obj) (eq? obj not-found))

 (define (make-number-store)
   (make-hashtable flat-hash flat-equal?))

 (define (make-obj-store)
   (make-eq-hashtable))

 (define number-store (make-parameter (make-number-store)))

 (define obj-store (make-parameter (make-obj-store)))

 (define smoosh +)

 (define (flat-vector-hash vec)
   (let loop ([n (vector-length vec)])
     (if (= n 0)
         1
         (smoosh (symbol-hash (vector-ref vec (- n 1)))
                 (loop (- n 1))))))

 (define (flat-number-hash obj)
   (mod obj (- (expt 2 29) 3)))

 (define (flat-hash obj)
   (flat-number-hash
    (cond [(null? obj) 381823]
          [(boolean? obj) (if obj 51991 77597)]
          [(pair? obj) (smoosh (symbol-hash (car obj)) (symbol-hash (cdr obj)))]
          [(symbol? obj) (symbol-hash obj)]
          [(real? obj) (inexact->exact (+ (numerator obj) (denominator obj)))]
          [(number? obj) obj]
          [(vector? obj) (flat-vector-hash obj)]
          [(procedure? obj) 1748131]
          [else (error obj "cannot hash obj type")])))

 (define (flat-equal? obj1 obj2)
   (cond [(eqv? obj1 obj2) #t]
         [(and (pair? obj1) (pair? obj2))
          (and (eq? (car obj1) (car obj2))
               (eq? (cdr obj1) (cdr obj2)))]
         [(and (vector? obj1) (vector? obj2)
               (= (vector-length obj1) (vector-length obj2)))
          (all (lambda (x) x)
               (vector->list (vector-map eq? obj1 obj2)))]
         [else #f]))

 (define (flat-obj->num flat-obj)
   (hashtable-ref/default (number-store)
                          flat-obj
                          (lambda ()
                            (let ([id (readable-gensym)])
                              (hashtable-set! (number-store) flat-obj id)
                              (hashtable-set! (obj-store) id flat-obj)
                              id))))

 (define (value-number? obj)
   (and (symbol? obj)
        (prefixed-symbol? obj '&)))


 ;; --------------------------------------------------------------------
 ;; One-step and recursive compression and expansion

 (define (make-typed-compressor is-type?)
   (lambda (obj)
     (assert (is-type? obj))
     (flat-obj->num obj)))

 (define compress-null (make-typed-compressor null?))

 (define compress-pair (make-typed-compressor pair?))

 (define compress-vector (make-typed-compressor vector?))

 (define compress-number (make-typed-compressor number?))

 (define compress-symbol (make-typed-compressor symbol?))

 (define compress-boolean (make-typed-compressor boolean?))

 (define (compress-procedure proc name)
   (hashtable-ref/default (number-store)
                          proc
                          (lambda ()
                            (let ([id (sym-append '&proc name)])
                              (hashtable-set! (number-store) proc id)
                              (hashtable-set! (obj-store) id proc)
                              id))))

 (define (compress-list ns)
   (assert (list? ns))
   (if (null? ns)
       (flat-obj->num '())
       (&cons (car ns) (compress-list (cdr ns)))))

 (define (compress-recursive obj)
   (assert (not (value-number? obj)))
   (cond [(null? obj) (flat-obj->num '())]
         [(pair? obj) (flat-obj->num (cons (compress-recursive (car obj))
                                           (compress-recursive (cdr obj))))]
         [(vector? obj) (flat-obj->num (vector-map compress-recursive obj))]
         [(symbol? obj) (flat-obj->num obj)]
         [(number? obj) (flat-obj->num obj)]
         [(boolean? obj) (flat-obj->num obj)]
         [(procedure? obj)
          (begin
            (display "Compressing procedure using gensym id...\n")
            (compress-procedure obj (readable-gensym)))]
         [else (error obj "compress-recursive: unknown object type")]))

 (define (&expand-step n)
   (assert (value-number? n))
   (hashtable-ref (obj-store) n not-found))

 (define (make-typed-expander is-type?)
   (lambda (n)
     (let ([v (&expand-step n)])
       (when (not (is-type? v))
             (pe "value number: " n "\n")
             (pe "flat hashtable result for n: " v "\n")
             (pe "fully expanded n: " (&expand-recursive n) "\n")
             (pe "expected type: " is-type? "\n")
             (assert (is-type? v)))
       v)))

 (define &expand-null (make-typed-expander null?))

 (define &expand-pair (make-typed-expander pair?))

 (define &expand-number (make-typed-expander number?))

 (define &expand-symbol (make-typed-expander symbol?))

 (define &expand-vector (make-typed-expander vector?))

 (define &expand-boolean (make-typed-expander boolean?))

 (define &expand-procedure (make-typed-expander procedure?))

 (define (&expand-list n)
   (let ([obj (hashtable-ref (obj-store) n not-found)])
     (cond [(pair? obj) (cons (car obj) (&expand-list (cdr obj)))]
           [(null? obj) '()]
           [else (error "&expand-list: number refers to non-list object")])))

 (define (&expand-recursive n)
   (let ([obj (hashtable-ref (obj-store) n not-found)])
     (if (not-found? obj)
         n
         (cond [(symbol? obj) (&expand-recursive obj)]
               [(vector? obj) (vector-map &expand-recursive obj)]
               [(pair? obj) (cons (&expand-recursive (car obj)) (&expand-recursive (cdr obj)))]
               [else obj]))))


 ;; --------------------------------------------------------------------
 ;; Operations on compressed data structures:

 (define (&not x)
   (compress-boolean (not (&expand-boolean x))))

 (define (&or . args)
   (compress-boolean (any (lambda (x) x) (map &expand-boolean args))))

 (define (&and . args)
   (compress-boolean (every (lambda (x) x) (map &expand-boolean args))))

 (define (&< a b)
   (compress-boolean (< (&expand-number a) (&expand-number b))))

 (define (&> a b)
   (compress-boolean (> (&expand-number a) (&expand-number b))))

 (define (&<= a b)
   (compress-boolean (<= (&expand-number a) (&expand-number b))))

 (define (&>= a b)
   (compress-boolean (>= (&expand-number a) (&expand-number b))))

 (define (&= a b)
   (compress-boolean (= (&expand-number a) (&expand-number b))))

 (define (&+ . args)
   (compress-number (apply + (map &expand-number args))))

 (define (&- a b)
   (compress-number (- (&expand-number a) (&expand-number b))))

 (define (&* a b)
   (compress-number (* (&expand-number a) (&expand-number b))))

 (define (&/ a b)
   (compress-number (/ (&expand-number a) (&expand-number b))))

 (define (&eq? n1 n2)
   (compress-boolean (eq? n1 n2)))

 (define (&eq?->b n1 n2)
   (eq? n1 n2))

 (define (&id n)
   n)

 (define (&list . args)
   (compress-list args))

 (define (&list-ref/n n i)
   (if (= i 0)
       (&car n)
       (&list-ref/n (&cdr n) (- i 1))))

 (define (&list-ref n i)
   (&list-ref/n n (&expand-number i)))

 (define (&cons n1 n2)
   (flat-obj->num (cons n1 n2)))

 (define (&reverse lst)
   (compress-list (reverse (&expand-list lst))))

 (define (&symbol?->b n)
   (let ([obj (hashtable-ref (obj-store) n not-found)])
     (and (not (not-found? obj))
          (symbol? obj)
          (not-found? (hashtable-ref (obj-store) obj not-found)))))

 (define (&symbol? n)
   (compress-boolean (&symbol?->b n)))

 (define (&vector . ns)
   (flat-obj->num (list->vector ns)))

 (define (&vector? n)
   (compress-boolean (&vector?->b n)))

 (define (&vector?->b n)
   (vector? (&expand-step n)))

 (define (&vector-ref n i)
   (vector-ref (&expand-vector n) i))

 (define (&vector-append n1 n2)
   (compress-vector
    (vector-append (&expand-vector n1)
                   (&expand-vector n2))))

 (define (&vector-length n)
   (vector-length
    (&expand-vector n)))

 (define (&vector-index proc n)
   (let ([m (&vector-length n)])
     (let loop ([i 0])
       (cond [(= i m) #f]
             [(proc (&expand-recursive (&vector-ref n i))) i]
             [else (loop (+ i 1))]))))

 (define (&null? n)
   (compress-boolean (&null?->b n)))

 (define (&null?->b n)
   (let ([obj (hashtable-ref (obj-store) n not-found)])
     (null? obj)))

 (define (&pair? n)
   (compress-boolean (&pair?->b n)))

 (define (&pair?->b n)
   (pair? (&expand-step n)))

 (define (&car n)
   (car (&expand-pair n)))

 (define (&cdr n)
   (cdr (&expand-pair n)))

 (define (&cadr n)
   (car (&expand-pair (&cdr n))))

 (define (&cddr n)
   (cdr (&expand-pair (&cdr n))))

 (define (&caddr n)
   (car (&expand-pair (&cddr n))))

 (define (&cdddr n)
   (cdr (&expand-pair (&cddr n))))

 (define (&cadddr n)
   (car (&expand-pair (&cdddr n))))

 (define (&cddddr n)
   (cdr (&expand-pair (&cdddr n))))

 (define (&tagged-list?->b n sym)
   (and (&pair?->b n)
        (let ([c (&car n)])
          (and (&symbol?->b c)
               (&eq?->b (&expand-symbol c) sym)))))

 (define &null (compress-null '()))

 (define &true (compress-boolean #t))

 (define &false (compress-boolean #f))

 )