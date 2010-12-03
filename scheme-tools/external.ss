#!r6rs

(library

 (scheme-tools external)

 (export delay
         force
         define/curry
         compose
         format
         void
         $
         match-lambda
         pretty-print
         gensym
         define/kw
         lambda/kw
         inexact->exact
         exact->inexact)

 (import (rnrs)
         (only (ikarus) pretty-print gensym inexact->exact exact->inexact format void)
         (only (rnrs r5rs) delay force)
         (only (xitomatl keywords) define/kw lambda/kw)
         (only (xitomatl curry) define/curry)
         (only (xitomatl control) compose)
         (only (xitomatl match) match-lambda))

 (define $ compose)

 )