#!r6rs

(library

 (scheme-tools external)

 (export $
         assert
         command-line-arguments
         compose
         console-input-port
         define/curry
         define/kw
         delay
         environment
         eval
         exact->inexact
         force
         format
         fprintf
         gensym
         inexact->exact
         lambda/kw
         make-parameter
         match-lambda
         modulo
         parameterize
         pretty-print
         system
         time
         void)

 (import (rnrs)
         (scheme-tools implementation-specific)
         (only (rnrs r5rs) delay force)
         (only (xitomatl keywords) define/kw lambda/kw)
         (only (xitomatl curry) define/curry)
         (only (xitomatl control) compose)
         (only (xitomatl match) match-lambda))

 (define $ compose)

 )