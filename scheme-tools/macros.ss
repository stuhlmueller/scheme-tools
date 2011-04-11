#!r6rs

(library

 (scheme-tools macros)

 (export opt-timeit)

 (import (rnrs)
         (scheme-tools implementation-specific))

 (define-syntax opt-timeit
   (syntax-rules ()
     [(opt-timeit test expr) (if test (time expr) expr)]))

 )