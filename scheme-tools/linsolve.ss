#!r6rs

;; Example:
;; (linsolve '((= (+ (* .5 x) (* .2 y)) 1.3)
;;             (= (+ (* .12 x) (* .83 y)) 4.3)))
;; => ((y . 5.099744245524297)
;;     (x . 0.5601023017902814))

;; Returns a single solution.

(library

 (scheme-tools linsolve)

 (export linsolve)

 (import (rnrs)
         (scheme-tools py-pickle))

 (define solver "linsolve")

 (define (strings->symbols sol)
   (map (lambda (kv)
          (cons (string->symbol (car kv))
                (cadr kv)))
        sol))

 (define (linsolve eqns)
   (let ([port (open-py-ports solver)])
     (for-each (lambda (eqn) (py-pickle port eqn))
               eqns)
     (py-pickle port 'solve)
     (strings->symbols (py-unpickle port))))

 )