#!r6rs

(library

 (scheme-tools math symbolic)

 (export s* s+ s- s/ make-symbolic)

 (import (rnrs)
         (scheme-tools readable-scheme))
 
 (define (make-symbolic op sym)
   (lambda args
     (if (all number? args)
         (apply op args)
         (cons sym args))))
 
 (define s* (make-symbolic * '*))
 
 (define s+ (make-symbolic + '+))

 (define s- (make-symbolic - '-))

 (define s/ (make-symbolic / '/))

 )