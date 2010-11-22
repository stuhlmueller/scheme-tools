#!r6rs

(library

 (scheme-tools readable-scheme)

 (export first
         rest)

 (import (rnrs))

 (define first car)

 (define rest cdr)

 )