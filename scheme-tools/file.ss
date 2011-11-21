#!r6rs

(library

 (scheme-tools file)

 (export read-file)
 
 (import (rnrs))

 (define (read-file filename)
   (with-input-from-file filename
     (lambda () (let loop ([out '()])
             (let ([in (read (current-input-port))])
               (if (eof-object? in)
                   (reverse out)
                   (loop (cons in out))))))))

 )
             

