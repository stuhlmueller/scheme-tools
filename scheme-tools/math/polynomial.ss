#!r6rs

;; Example:
;; (polysolve '((= (+ (* .5 x) (* .2 y)) 1.3)
;;              (= (+ (* .12 x) (* .83 y)) 4.3)))
;; => (((y . 5.0997442455243)
;;      (x . 0.560102301790281)))

;; Returns a set of solutions.

(library

 (scheme-tools math polynomial)

 (export polysolve
         polysolve/unique)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (scheme-tools implementation-specific)
         (scheme-tools py-pickle))

 (define solver "polysolve")

 (define (strings->symbols sol)
   (map (lambda (kv)
          (cons (string->symbol (car kv))
                (cadr kv)))
        sol))

 (define (polysolve eqns)
   (let ([port (open-py-ports solver)])
     (for-each (lambda (eqn) (py-pickle port eqn))
               eqns)
     (py-pickle port 'solve)
     (map strings->symbols (py-unpickle port))))

 (define (polysolve/unique eqns)
   (let ([solutions (polysolve eqns)])
     (when (not (= (length solutions) 1))
           (display "polysolve/unique failed.\neqns:\n")
           (map pretty-print eqns)
           (display "solutions:\n")
           (map pretty-print solutions)
           (assert (= (length solutions) 1)))
     (first solutions)))

 )