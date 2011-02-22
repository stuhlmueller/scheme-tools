#!r6rs

;; Solve a system of polynomial equations by iteratively solving
;; linear systems, thus getting monotonically improving lower
;; bounds. This is possible if the system of polynomials corresponds
;; to a program that halts with probability 1.

;; Example:
;;
;; (linsolve/iterative '((= x (+ (* .5 x) (* .2 y) .3))
;;                       (= y (+ (* .12 x) (* .3 y) .7)))
;;                     20)
;; ((y . 1.1840356930321616) (x . 1.0735879063861111))

(library

 (scheme-tools math iterate)

 (export linsolve/iterative)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools srfi-compat :1)
         (scheme-tools hash)
         (scheme-tools math linear))

 (define operators '(+ - = * /))

 (define (operator? obj)
   (any (lambda (op) (eq? op obj))
        operators))

 (define (variable? obj)
   (and (symbol? obj) (not (operator? obj))))

 (define (replace-variable obj hash-table default)
   (hash-table-ref/default hash-table obj default))

 (define (replace-variables obj hash-table default)
   (cond [(pair? obj) (pair (replace-variables (first obj) hash-table default)
                            (replace-variables (rest obj) hash-table default))]
         [(variable? obj) (replace-variable obj hash-table default)]
         [else obj]))

 (define (generate-linear-eqns poly-eqns estimated-solution)
   (let ([estimate-table (alist->hash-table estimated-solution eq?)])
     (map (lambda (poly-eqn)
            (append (take poly-eqn 2)
                    (replace-variables (drop poly-eqn 2) estimate-table 0.0)))
          poly-eqns)))

 (define (linsolve/iterative poly-eqns n)
   (let loop ([n n]
              [estimate '()])
     (if (= n 0)
         estimate
         (let ([linear-eqns (generate-linear-eqns poly-eqns estimate)])
           (loop (- n 1)
                 (linsolve linear-eqns))))))

 )