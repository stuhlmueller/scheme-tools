#!r6rs

;; Convert equation to function

(library

 (scheme-tools math eval)

 (export eqns->func
         eqns->ad-func)

 (import (rnrs)
         (xitomatl keywords)
         (scheme-tools srfi-compat :1)
         (scheme-tools math AD)
         (scheme-tools math math)
         (scheme-tools))

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

 (define (dmax . lst)
   (if (null? lst)
       (error lst "dmax got empty list")
       (let loop ([lst (cdr lst)]
                  [mx (car lst)])
         (if (null? lst)
             mx
             (if (d> (car lst) mx)
                 (loop (cdr lst) (car lst))
                 (loop (cdr lst) mx))))))

 (define (dlogsumexp . log-vals)
   (let ([max-log-val (apply dmax log-vals)])
     (if (equal? max-log-val -inf.0)
         -inf.0
         (d+ (dlog (apply d+ (map (lambda (val) (dexp (d- val max-log-val))) log-vals)))
             max-log-val))))

 (define (application-expr->operator/ad app)
   (let ([operator-expr (first app)])
     (cond [(eq? operator-expr 'logsumexp) dlogsumexp]
           [(eq? operator-expr '+) d+]
           [(eq? operator-expr '*) d*]
           [(eq? operator-expr '-) d-]
           [(eq? operator-expr '/) d/]
           [(eq? operator-expr 'expt) dexpt]
           [(eq? operator-expr 'exp) dexp]
           [else (error operator-expr "not found")])))

 (define (application-expr->operator/no-ad app)
   (let ([operator-expr (first app)])
     (cond [(eq? operator-expr 'logsumexp) logsumexp]
           [(eq? operator-expr '+) +]
           [(eq? operator-expr '*) *]
           [(eq? operator-expr '-) -]
           [(eq? operator-expr '/) /]
           [(eq? operator-expr 'expt) expt]
           [(eq? operator-expr 'exp) exp]
           [else (error operator-expr "not found")])))

 (define (application-expr->operator app ad)
   (if ad
       (application-expr->operator/ad app)
       (application-expr->operator/no-ad app)))

 (define application-expr->vars rest)

 (define application-expr? list?)

 (define (resolve-symbol sym var-indizes)
   (cond [(eq? sym 'LOG-PROB-0) (lambda (vars) LOG-PROB-0)]
         [(eq? sym 'LOG-PROB-1) (lambda (vars) LOG-PROB-1)]
         [else
          (let ([index (hashtable-ref var-indizes sym #f)])
            (lambda (vars) (vector-ref vars index)))]))

 (define (eqn-body->func body var-indizes ad)
   (let parse ([body body])
     (cond [(number? body) (lambda (vars) body)]
           [(symbol? body) (resolve-symbol body var-indizes)]
           [(application-expr? body)
            (let ([operand-fns (map parse (application-expr->vars body))]
                  [operator (application-expr->operator body ad)])
              (lambda (vars) (apply operator (map (lambda (f) (f vars)) operand-fns))))])))

 (define/kw (eqns->func eqns [ad :default #f])
   (let* ([var-indizes (eqns->var-indizes eqns)]
          [fns (vector-map (lambda (eqn) (eqn-body->func (eqn->body eqn) var-indizes ad))
                           (list->vector eqns))])
     (lambda (vars)
       (vector-map (lambda (f) (f vars)) fns))))

 (define (eqns->ad-func eqns)
   (eqns->func eqns 'ad #t))

 )