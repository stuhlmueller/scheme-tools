#!r6rs

(library

 (scheme-tools debug)

 (export assert*
         define/debug
         define/count
         show-debug-counts
         reset-debug-counts!
         debug-mode)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (scheme-tools readable-scheme)
         (scheme-tools implementation-specific))

 (define debug-mode (make-parameter #f))

 (define (print-arg arg)
   (pe "\n        " (car arg) ": ")
   (apply pe (cdr arg)))

 (define (print-call g0 name syms vals)
   (pe "\n[" g0 "] " (cons name syms))
   (for-each print-arg
             (zip syms vals))
   (pe "\n"))

 (define (print-return g0 v)
   (pe "[" g0 "] => " v "\n"))

 (define debug-counter (get-counter))

 (define (with-debug-info name syms vals ret-thunk)
   (let ([g0 (debug-counter)])
     (begin
       (when (debug-mode)
             (print-call g0 name syms vals))
       (let ([v (ret-thunk)])
         (when (debug-mode)
               (print-return g0 v))
         v))))

 (define-syntax define/debug
   (syntax-rules ()
     [(_ (name a1 a2 ...) . bodies)
      (define (name a1 a2 ...)
        (with-debug-info 'name '(a1 a2 ...) (list a1 a2 ...) (lambda () (begin . bodies))))]
     [(_ (name a1 a2 ... . an) . bodies)
      (define (name a1 a2 ... . an)
        (with-debug-info 'name '(a1 a2 ... an) (list a1 a2 ... an) (lambda () (begin . bodies))))]
     [(_ (name) . bodies)
      (define (name)
        (with-debug-info 'name '() '() (lambda () (begin . bodies))))]
     [(_ (name . an) . bodies)
      (define (name . an)
        (with-debug-info 'name '(an) (list an) (lambda () (begin . bodies))))]))

 (define call-counts
   (make-eq-hashtable))

 (define (with-counter name thunk)
   (let ([count (hashtable-ref call-counts
                               name
                               0)])
     (hashtable-set! call-counts name (+ count 1))
     (thunk)))

 (define-syntax define/count
   (syntax-rules ()
     [(_ (name a1 a2 ...) . bodies)
      (define (name a1 a2 ...)
        (with-counter 'name (lambda () (begin . bodies))))]
     [(_ (name a1 a2 ... . an) . bodies)
      (define (name a1 a2 ... . an)
        (with-counter 'name (lambda () (begin . bodies))))]
     [(_ (name) . bodies)
      (define (name)
        (with-counter 'name (lambda () (begin . bodies))))]
     [(_ (name . an) . bodies)
      (define (name . an)
        (with-counter 'name (lambda () (begin . bodies))))]))

 (define (reset-debug-counts!)
   (set! call-counts (make-eq-hashtable)))

 (define (show-debug-counts)
   (let-values ([(keys vals) (hashtable-entries call-counts)])
     (vector-map (lambda (k v) (pe k ": " v "\n"))
                 keys
                 vals)))

 (define-syntax assert*
   (syntax-rules ()
     [(_ condition thunk)
      (let ([v condition])
        (when (not v)
              (thunk)
              (display "\n")
              (error 'condition "assertion failed")))]))

 )