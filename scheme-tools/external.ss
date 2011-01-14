#!r6rs

(library

 (scheme-tools external)

 (export $
         assert
         compose
         define/curry
         define/kw
         delay
         exact->inexact
         force
         format
         fprintf
         gensym
         inexact->exact
         lambda/kw
         match-lambda
         pretty-print
         system
         void)

 (import (rnrs)
         (only (ikarus)
               assert
               pretty-print
               gensym
               inexact->exact
               exact->inexact
               format
               void
               system
               fprintf)
         (only (rnrs r5rs) delay force)
         (only (xitomatl keywords) define/kw lambda/kw)
         (only (xitomatl curry) define/curry)
         (only (xitomatl control) compose)
         (only (xitomatl match) match-lambda))

 (define $ compose)

 )