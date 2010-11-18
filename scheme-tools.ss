#!r6rs

(library

 (scheme-tools)

 (export delay
         force
         define/curry
         compose
         match-lambda
         pretty-print
         gensym)

 (import (only (ikarus) pretty-print gensym)
         (only (rnrs r5rs) delay force)
         (only (xitomatl curry) define/curry)
         (only (xitomatl control) compose)
         (only (xitomatl match) match-lambda))

 )

 
         
         