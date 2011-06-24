#!r6rs

;; Solve a system of (polynomial) equations by iteration on the
;; solutions, starting from 0, thus getting monotonically improving
;; lower bounds. This is possible if the system of polynomials
;; corresponds to a program that halts with probability 1.

;; Example:
;; 
;; (define eqns '((= x (+ (* .5 x) (* .2 y) .3))
;;                (= y (+ (* .12 x) (* .3 y) .7))))
;; (iterate/plain eqns 0.0)
;; -> ((x . 1.0736196319018403) (y . 1.184049079754601))

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
               old-vals
               new-vals)))

 (define (stop? n max-iters vals new-vals)
   (if (> n max-iters)
       (begin
         (pe "Iterator exceeded " max-iters " iterations! Delta: "
             (delta vals new-vals) "\n")
         true)
       false))
 
 (define/kw (iterate start update target-delta [max-iters :default 10000000])
   (let loop ([n 0]
              [vals start])
     (let ([new-vals (update vals)])
       (if (or (<= (delta vals new-vals) target-delta)
               (stop? n max-iters vals new-vals))
           new-vals
           (loop (+ n 1)
                 new-vals)))))

 
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
                                         `[,var-name (list-ref vars ,i)])
                                       var-names)]
          [iterator-expr `(lambda (vars)
                            (let ,var-bindings
                              (list ,@(map eqn->body eqns))))])
     (eval iterator-expr env))) 
 
 (define/kw (iterate/eqns eqns
                          target-delta
                          [max-iters :default 10000]
                          [start-value :default -inf.0]
                          [env :default (environment '(rnrs))])
   (named-vals eqns
               (iterate (make-list (length eqns) start-value)
                        (eqns->func eqns env)
                        target-delta
                        'max-iters max-iters)))
 
 )