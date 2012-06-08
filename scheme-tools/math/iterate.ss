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
         (scheme-tools profile)
         (scheme-tools external)
         (scheme-tools readable-scheme)
         (scheme-tools srfi-compat :1)
         (scheme-tools math math)
         (xitomatl keywords))


 ;; Function iterator

 ;; Return 0.0 when two identical symbols (like -inf.0, +inf.0,
 ;; -nan.0, +nan.0) are given.
 (define (extended- a b)
   (if (eqv? a b)
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


 ;; Helper functions to convert equation to function

 (define eqn->var second)

 (define (eqn->body eqn)
   (first (drop eqn 2)))

 (define (eqns->var-indizes eqns)
   (let ([i 0]
         [bindings (make-eq-hashtable)])
     (for-each (lambda (eqn)
                 (let ([var-name (second eqn)])
                   (hashtable-set! bindings var-name i)
                   (set! i (+ i 1))))
               eqns)
     bindings))

 (define (application-expr->operator app env)
   (let ([operator-expr (first app)])
     (cond [(eq? operator-expr 'logsumexp) logsumexp]
           [(eq? operator-expr '+) +]
           [else (error operator-expr "not found")])))

 (define application-expr->vars rest)

 (define application-expr? list?)

 (define (resolve-symbol sym var-indizes env)
   (cond [(eq? sym 'LOG-PROB-0) (lambda (vars) LOG-PROB-0)]
         [(eq? sym 'LOG-PROB-1) (lambda (vars) LOG-PROB-1)]
         [else
          (let ([index (hashtable-ref var-indizes sym #f)])
            (lambda (vars) (vector-ref vars index)))]))

 (define (eqn-body->func body var-indizes env)
   (let parse ([body body])
     (cond [(number? body) (lambda (vars) body)]
           [(symbol? body) (resolve-symbol body var-indizes env)]
           [(application-expr? body)
            (let ([operand-fns (map parse (application-expr->vars body))]
                  [operator (application-expr->operator body env)])
              (lambda (vars) (apply operator (map (lambda (f) (f vars)) operand-fns))))])))

 (define (eqns->func eqns env)
   (let* ([var-indizes (eqns->var-indizes eqns)]
          [fns (vector-map (lambda (eqn)
                             (eqn-body->func (eqn->body eqn) var-indizes env))
                           (list->vector eqns))])
     (lambda (vars)
       (vector-map (lambda (f) (f vars)) fns))))


 ;; Equation iterator (based on function iterator)

 (define (named-vals eqns vals)
   (map (lambda (eqn val) (pair (eqn->var eqn) val))
        eqns
        vals))

 (define/kw (iterate/eqns eqns
                          target-delta
                          [max-iters :default 10000]
                          [start-value :default -inf.0]
                          [env :default (environment '(rnrs))])
   (let ([func (eqns->func eqns env)])
     (let-values ([(vals final-delta) (iterate (make-vector (length eqns) start-value)
                                               func
                                               target-delta
                                               'max-iters max-iters)])
       (values (named-vals eqns (vector->list vals))
               final-delta))))

 )