#!r6rs

(library

 (scheme-tools math newton)

 (export newton)

 (import (rnrs)
         (xitomatl keywords)
         (scheme-tools)
         (scheme-tools srfi-compat :1)
         (scheme-tools math AD)
         (scheme-tools math eval)
         (scheme-tools math partials)
         (scheme-tools math linear)
         (scheme-tools math iterate))

 (define (zero-equations equations)
   (map (lambda (eqn)
          `(= ,(second eqn)
              (- ,(third eqn) ,(second eqn))))
        equations))

 (define (newton-linear-equations vars D0 F0)
   (list (vector->list (vector-map vector->list D0))
         (vector->list (vector-map - F0))))

 (define/kw (newton eqns [init-val :default 0.0] [tolerance :default 0.0001] [max-iterations :default 30])
   (let* ([f (eqns->ad-func (zero-equations eqns))]
          [vars (map second eqns)]
          [y-dim (length eqns)]
          [Df (partials f y-dim)]
          [v0 (make-vector y-dim init-val)])
     (let loop ([v0 v0]
                [iters 0])
       (let* ([D0 (Df v0)]
              [F0 (f v0)]
              [lineqns (newton-linear-equations vars D0 F0)]
              [linsol (linsolve (first lineqns) (second lineqns))]
              [deltav (list->vector linsol)]
              [delta (abs (apply + (vector->list deltav)))])
         (let ([v1 (vector-map + v0 deltav)])
           (if (> iters max-iterations)
               (begin
                 (pen "newton: >max-iterations, delta " delta)
                 (map cons vars (vector->list v1)))
               (if (< delta tolerance)
                   (map cons vars (vector->list v1))
                   (loop v1 (+ iters 1)))))))))

 )
