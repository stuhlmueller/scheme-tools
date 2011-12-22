#!r6rs

;; Solve a system of (polynomial) equations by iteration on the
;; solutions, starting from 0, thus getting monotonically improving
;; lower bounds. This is possible if the system of polynomials
;; corresponds to a program that halts with probability 1.

;; Example:
;; 
;; (define eqns '((= x (+ (* .5 x) (* .2 y) .3))
;;                (= y (+ (* .12 x) (* .3 y) .7))))
;; (iterate/eqns eqns 0.0 'start-value 0.0)
;; -> ((x . 1.0736196319018403) (y . 1.184049079754601))
;; -> 0.0

(library

 (scheme-tools math iterate)

 (export iterate
         iterate/eqns)

 (import (rnrs)
         (scheme-tools)         
         (scheme-tools external)
         (scheme-tools readable-scheme)
         (scheme-tools srfi-compat :1)
         (xitomatl keywords))


 ;; Function iterator

 ;; Return 0.0 when two identical symbols (like -inf.0, +inf.0,
 ;; -nan.0, +nan.0) are given.
 (define (extended- a b)
   (if (eq? a b)
       0.0
       (- a b)))
 
 (define (delta old-vals new-vals)
   (apply max
          (map (lambda (old new) (abs (extended- old new)))
               (vector->list old-vals)
               (vector->list new-vals))))
 
 (define/kw (iterate start update target-delta [max-iters :default 10000])
   (let loop ([n 0]
              [vals start])
     (let* ([new-vals (update vals)]
            [cur-delta (delta vals new-vals)])
       (if (or (<= cur-delta target-delta)
               (> n max-iters))
           (values new-vals cur-delta)
           (loop (+ n 1) new-vals)))))
 
 
 ;; Equation iterator (based on function iterator)
 
 (define eqn->var second)
 
 (define (eqn->body eqn)
   (first (drop eqn 2))) 

 (define (named-vals eqns vals)
   (map (lambda (eqn val) (pair (eqn->var eqn) val))
        eqns
        vals))

 (define/kw (eqns->func eqns env)
   (let* ([var-names (map second eqns)]
          [var-bindings (map-enumerate (lambda (i var-name)
                                         `[,var-name (vector-ref vars ,i)])
                                       var-names)]
          [iterator-expr `(lambda (vars)
                            (let ,var-bindings
                              (vector ,@(map eqn->body eqns))))]
          [func (eval iterator-expr env)])
     func))
 
 (define/kw (iterate/eqns eqns
                          target-delta
                          [max-iters :default 10000]
                          [start-value :default -inf.0]
                          [env :default (environment '(rnrs))])
   (let-values ([(vals final-delta) (iterate (make-vector (length eqns) start-value)
                                             (eqns->func eqns env)
                                             target-delta
                                             'max-iters max-iters)])
     (values (named-vals eqns (vector->list vals))
             final-delta)))
 
 )