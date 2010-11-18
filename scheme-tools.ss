#!r6rs

(library

 (scheme-tools)

 (export delay
         force
         define/curry
         compose
         match-lambda
         pretty-print
         gensym
         symbol-maker)

 (import (rnrs)
         (only (ikarus) pretty-print gensym)
         (only (rnrs r5rs) delay force)
         (only (xitomatl curry) define/curry)
         (only (xitomatl control) compose)
         (only (xitomatl match) match-lambda))

 (define (symbol-maker sym)
   (let ([s 0])
     (lambda ()
       (begin
         (set! s (+ s 1))
         (string->symbol (string-append (symbol->string sym) (number->string s)))))))

 )

 
         
         