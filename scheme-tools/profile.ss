#!r6rs

(library

 (scheme-tools profile)

 (export get-runtime
         get-runtime&value)

 (import (rnrs)
         (srfi :19)
         (scheme-tools time))

 (define (get-runtime proc)
   (let ([start-time (current-time)])
     (proc)
     (time-diff (current-time) start-time)))

 (define (get-runtime&value proc)
   (let ([start-time (current-time)])
     (let ([result (proc)])
       (values (time-diff (current-time) start-time)
               result))))

 )