#!r6rs

(library

 (scheme-tools readable-scheme)

 (export rest
         pair
         true
         false
         true?
         false?
         sum
         all
         tagged-list?
         symbol-maker
         pe
         call&return
         ->string
         ->string:n)

 (import (rnrs))

 (define true #t)

 (define false #f)

 (define rest cdr)

 (define pair cons)

 (define (true? obj)
   (eq? obj #t))

 (define (false? obj)
   (eq? obj #f))

 (define (sum vals)
   (apply + vals))

 (define (all proc lst)
   (if (null? lst)
       #t
       (and (proc (car lst))
            (all proc (cdr lst)))))

 (define (tagged-list? obj tag)
   (and (list? obj)
        (not (null? obj))
        (eq? (car obj) tag)))

 (define (symbol-maker sym)
   (let ([s 0])
     (lambda ()
       (begin
         (set! s (+ s 1))
         (string->symbol (string-append (symbol->string sym) (number->string s)))))))

 (define (pe . args)
   (for-each display args))

 (define (call&return proc arg)
   (proc arg)
   arg)

 (define (->string val)
   (let-values ([(string-port extractor) (open-string-output-port)])
     (write val string-port)
     (extractor)))
         
 (define (->string:n val n)
   (let ([string (->string val)])
     (if (<= (string-length string) n)
         string
         (string-append (substring string 0 n)
                        "..."))))

 )