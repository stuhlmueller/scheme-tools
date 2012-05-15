#!r6rs

(library

 (scheme-tools profile)

 (export get-runtime)

 (import (rnrs)
         (srfi :19)
         (scheme-tools time))

 (define (get-runtime proc)
   (let ([start-time (current-time)])
     (proc)
     (time-diff (current-time) start-time)))

 )