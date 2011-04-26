#!r6rs

;; Solve a system of polynomial equations by iteration on the
;; solutions, starting from 0, thus getting monotonically improving
;; lower bounds. This is possible if the system of polynomials
;; corresponds to a program that halts with probability 1.

;; Example:
;; 
;; (define eqns '((= x (+ (* .5 x) (* .2 y) .3))
;;                (= y (+ (* .12 x) (* .3 y) .7))))
;; (iterate eqns 0.0)
;; -> ((x . 1.0736196319018403) (y . 1.184049079754601))

(library

 (scheme-tools math iterate)

 (export iterate)

 (import (rnrs)
         (scheme-tools)         
         (scheme-tools external)
         (scheme-tools readable-scheme)
         (scheme-tools srfi-compat :1)
         (xitomatl keywords))

 (define eqn->var second)
 
 (define (eqn->body eqn)
   (first (drop eqn 2)))

 (define (eqns->iterator eqns)
   (let* ([var-names (map second eqns)]
          [var-bindings (map-enumerate (lambda (i var-name)
                                         `[,var-name (list-ref vars ,i)])
                                       var-names)]
          [iterator-expr `(lambda (vars)
                            (let ,var-bindings
                              (list ,@(map eqn->body eqns))))])
     (eval iterator-expr
           (environment '(rnrs)))))

 (define (delta old-vals new-vals)
   (apply max
          (map (lambda (old new) (abs (- old new)))
               old-vals
               new-vals)))

 (define (stop? n max-iters vals new-vals)
   (if (> n max-iters)
       (begin
         (pe "Iterator exceeded " max-iters " iterations! Delta: "
             (delta vals new-vals) "\n")
         true)
       false))
 
 (define/kw (iterate eqns d [max-iters :default 10000000])
   (let ([iterator (eqns->iterator eqns)])
     (let loop ([n 0]
                [vals (map (lambda (eqn) 0.0) eqns)])
       (let ([new-vals (iterator vals)])
         (if (or (<= (delta vals new-vals) d)
                 (stop? n max-iters vals new-vals))
             (map (lambda (eqn val)
                    (pair (eqn->var eqn) val))
                  eqns
                  new-vals)
             (loop (+ n 1)
                   new-vals))))))
 
 )