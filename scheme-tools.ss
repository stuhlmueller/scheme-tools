#!r6rs

(library

 (scheme-tools)

 (export delay
         force
         define/curry
         compose
         $
         match-lambda
         pretty-print
         gensym
         symbol-maker
         rest
         pair
         true
         false
         true?
         false?
         define/kw
         lambda/kw
         sum
         all
         tagged-list?
         inexact->exact
         exact->inexact)

 (import (rnrs)
         (scheme-tools readable-scheme)
         (scheme-tools external))

 )