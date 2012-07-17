#!r6rs

;; A hashtable-based data structure for discrete log probability distributions

(library

 (scheme-tools math distributions)

 (export make-empty-dist
         empty-dist?
         make-dist
         copy-dist
         dist-vals
         dist-ps
         dist-vals&ps
         sample-dist
         get-dist-prob
         set-dist-prob!
         dist-mass)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (scheme-tools hashtable)
         (scheme-tools math math))

 (define (make-empty-dist)
   (make-eq-hashtable))

 (define (empty-dist? dist)
   (= (hashtable-size dist) 0))

 (define (make-dist vals probs)
   (let ([dist (make-empty-dist)])
     (vector-for-each (lambda (val prob) (hashtable-set! dist val prob))
                      vals
                      probs)
     dist))

 (define (dist-vals dist)
   (hashtable-keys dist))

 (define (dist-ps dist)
   (hashtable-values dist))

 (define (dist-vals&ps dist)
   (hashtable-entries dist))

 (define (copy-dist dist)
   (let-values ([(vals ps) (dist-vals&ps dist)])
     (make-dist vals ps)))

 (define (sample-dist dist)
   (assert (not (= (dist-mass dist) LOG-PROB-0)))
   (let-values ([(vals ps) (dist-vals&ps dist)])
     (if (= (vector-length vals) 1)
         (vector-ref vals 0)
         (multinomial (vector->list vals)
                      (map exp (vector->list ps))))))

 (define (get-dist-prob dist val)
   (hashtable-ref dist val LOG-PROB-0))

 (define (set-dist-prob! dist val p)
   (hashtable-set! dist val p))

 (define (dist-mass dist)
   (if (empty-dist? dist)
       LOG-PROB-0
       (apply logsumexp (vector->list (dist-ps dist)))))

 )

