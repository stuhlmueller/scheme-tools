#!r6rs

(library

 (scheme-tools time)

 (export time-diff
         time-wait)

 (import (rnrs)
         (srfi :19)
         (scheme-tools external))

 (define (time-diff now last)
   (let ([diff (time-difference now last)])
     (exact->inexact (+ (time-second diff)
                        (/ (time-nanosecond diff) 1000000000)))))

 (define (time-wait span)
   (let ([init (current-time)])
     (let loop ([now (current-time)])
       (if (< (time-diff now init) span)
           (loop (current-time))
           'done))))

 )