#!r6rs

;; A hashtable-based data structure for discrete log probability distributions.

;; Each distribution is composed of a sequence of probability
;; fragments. It is possible to have more than one fragment per value.

(library

 (scheme-tools math fragmented-distributions)

 (export make-fragment
         copy-fdist
         empty-fdist?
         fdist-add-fragment!
         fdist-table
         fdist-fragments
         fdist-mass
         fdist-ps
         fdist-vals
         fdist-vals&ps
         fragment-id
         fragment-prob
         fragment-value
         fragments->fdist
         get-fdist-prob
         make-empty-fdist
         sample-fdist-fragment)

 (import (rnrs)
         (scheme-tools external)
         (scheme-tools readable-scheme)
         (scheme-tools srfi-compat :1)
         (scheme-tools hashtable)
         (scheme-tools math math)
         (scheme-tools math distributions))

 (define-record-type fdist
   (fields table (mutable fragments)))

 (define (make-empty-fdist)
   (make-fdist (make-eq-hashtable)
               '()))

 (define (fragments->fdist fragments)
   (let ([fdist (make-empty-fdist)])
     (for-each (lambda (fragment) (fdist-add-fragment! fdist fragment)
                  fragments)
               (reverse fragments))
     fdist))

 (define (empty-fdist? fdist)
   (null? (fdist-fragments fdist)))

 (define (copy-list lst)
   (map (lambda (x) x) lst))

 (define (copy-fdist fdist)
   (make-fdist (hashtable-copy (fdist-table fdist))
               (copy-list (fdist-fragments fdist))))

 (define (fdist-vals fdist)
   (hashtable-keys (fdist-table fdist)))

 (define (fdist-ps fdist)
   (hashtable-values (fdist-table fdist)))

 (define (fdist-vals&ps fdist)
   (hashtable-entries (fdist-table fdist)))

 (define (make-fragment v p . maybe-id)
   (let ([id (if (null? maybe-id) (gensym) (car maybe-id))])
     (vector v p id)))

 (define (fragment-value fragment)
   (vector-ref fragment 0))

 (define (fragment-prob fragment)
   (vector-ref fragment 1))

 (define (fragment-id fragment)
   (vector-ref fragment 2))

 (define (sample-fdist-fragment fdist)
   (assert (not (= (fdist-mass fdist) LOG-PROB-0)))
   (let* ([fragments (fdist-fragments fdist)]
          [sampled-fragment (multinomial fragments
                                         (map (lambda (fragment) (exp (fragment-prob fragment)))
                                              fragments))])
     sampled-fragment))

 (define (get-fdist-prob fdist val)
   (hashtable-ref (fdist-table fdist) val LOG-PROB-0))

 (define (fdist-add-fragment! fdist fragment)
   (fdist-fragments-set! fdist (cons fragment (fdist-fragments fdist)))
   (let ([table (fdist-table fdist)])
     (hashtable-set! table
                     (fragment-value fragment)
                     (logsumexp (hashtable-ref table (fragment-value fragment) LOG-PROB-0)
                                (fragment-prob fragment)))))

 (define (fdist-mass fdist)
   (if (empty-fdist? fdist)
       LOG-PROB-0
       (apply logsumexp (vector->list (fdist-ps fdist)))))

 )