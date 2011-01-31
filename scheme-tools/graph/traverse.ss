#!r6rs

(library

 (scheme-tools graph traverse)

 (export traverse)

 (import (rnrs))

 (define (traverse start next combine stop? default)
   (if (stop? start)
       default
       (combine start
                (map (lambda (obj) (traverse obj next combine stop? default))
                     (next start)))))

 )