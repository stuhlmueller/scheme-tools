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
         format
         gensym
         system
         fprintf
         symbol-maker
         get-counter
         sym+num
         sym+num->num
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
         pe
         call&return
         tagged-list?
         inexact->exact
         exact->inexact
         repl
         ->string
         ->string:n
         time-diff
         time-wait)

 (import (rnrs)
         (scheme-tools repl)
         (scheme-tools time)
         (scheme-tools readable-scheme)
         (scheme-tools external))

 )