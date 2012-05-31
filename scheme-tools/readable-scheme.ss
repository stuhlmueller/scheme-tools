#!r6rs

(library

 (scheme-tools readable-scheme)

 (export ->string
         ->string:n
         alist-map
         all
         call&return
         false
         false?
         get-counter
         identity
         opt
         pair
         pe
         pen
         pp
         ppe
         prefixed-string?
         prefixed-symbol?
         repeat
         sym+num
         sym+num->num
         sym-append
         symbol-maker
         tagged-list?
         true
         true?
         rest
         void?)

 (import (scheme-tools srfi-compat :1)
         (scheme-tools external)
         (rnrs))

 (define true #t)

 (define false #f)

 (define rest cdr)

 (define pair cons)

 (define (true? obj)
   (eq? obj #t))

 (define (false? obj)
   (eq? obj #f))

 (define (all proc lst)
   (if (null? lst)
       #t
       (and (proc (car lst))
            (all proc (cdr lst)))))

 (define (tagged-list? obj tag)
   (and (list? obj)
        (not (null? obj))
        (eq? (car obj) tag)))

 (define (sym+num sym num)
   (string->symbol
    (string-append
     (symbol->string sym)
     (number->string num))))

 (define (sym-append . syms)
   (string->symbol (apply string-append (map ->string syms))))

 (define (sym+num->num sn)
   (string->number
    (list->string
     (reverse
      (take-while char-numeric?
                  (reverse
                   (string->list
                    (symbol->string sn))))))))

 (define (prefixed-string? obj pre)
   (assert (string? obj))
   (assert (string? pre))
   (and (>= (string-length obj) (string-length pre))
        (equal? pre (substring obj 0 (string-length pre)))))

 (define (prefixed-symbol? obj pre)
   (assert (symbol? pre))
   (assert (symbol? obj))
   (prefixed-string? (symbol->string obj)
                     (symbol->string pre)))

 (define (symbol-maker sym)
   (let ([counter (get-counter)])
     (lambda () (sym+num sym (counter)))))

 (define (get-counter)
   (let ([s 0])
     (lambda ()
       (begin
         (set! s (+ s 1))
         s))))

 (define (pp? obj)
   (tagged-list? obj 'pp))

 (define (pp obj)
   (list 'pp obj))

 (define pp->obj second)

 (define (pen . args)
   (begin
     (apply pe args)
     (display "\n")))

 (define (pe . args)
   (if (null? args)
       #f
       (let ([value (first args)])
         (if (pp? value)
             (pretty-print (pp->obj value))
             (display value))
         (apply pe (rest args)))))

 (define (ppe . args)
   (apply pe (map pp args)))

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

 (define (repeat n proc)
   (if (= n 0)
       '()
       (cons (proc) (repeat (- n 1) proc))))

 (define (identity x)
   x)

 (define (opt arg default)
   (if (null? arg)
       default
       (car arg)))

 (define (void? obj)
   (eq? obj (void)))

 (define (alist-map proc alist)
   (map (lambda (x)
          (proc (car x) (cdr x)))
        alist))

 )