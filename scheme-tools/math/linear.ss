#!r6rs

;; Example:
;; (linsolve '((= (+ (* .5 x) (* .2 y)) 1.3)
;;             (= (+ (* .12 x) (* .83 y)) 4.3)))
;; => ((y . 5.099744245524297)
;;     (x . 0.5601023017902814))

;; Returns a single solution.

(library

 (scheme-tools math linear)

 (export linsolve)

 (import (rnrs)
         (scheme-tools py-pickle))

 (define solver "linsolve")

 (define (linsolve A b)
   (let ([port (open-py-ports solver)])
     (py-pickle port A)
     (py-pickle port b)
     (py-unpickle port)))

 )