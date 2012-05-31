#!r6rs

;; Based on:
;; http://en.literateprograms.org/Merge_sort_(Scheme)

(library

 (scheme-tools sort)

 (export median
         merge-sort
         string-sort)

 (import (rnrs)
         (scheme-tools readable-scheme)
         (scheme-tools implementation-specific))

 (define split-h
   (lambda (ls ls1 ls2)
     (cond
      [(if (null? ls)
           #t
           (if (null? (cdr ls))
               #t
               #f))
       (cons (reverse ls2) ls1)]
      [else (split-h (cdr (cdr ls))
                     (cdr ls1) (cons (car ls1) ls2))])))

 (define (split ls)
   (split-h ls ls '()))

 (define (merge pred ls1 ls2)
   (cond
    [(null? ls1) ls2]
    [(null? ls2) ls1]
    [(pred (car ls1) (car ls2))
     (cons (car ls1) (merge pred (cdr ls1) ls2))]
    [else (cons (car ls2) (merge pred ls1 (cdr ls2)))]))

 (define (merge-sort pred ls)
   (cond
    [(null? ls) ls]
    [(null? (cdr ls)) ls]
    [else (let ([splits (split ls)])
            (merge pred
                   (merge-sort pred (car splits))
                   (merge-sort pred (cdr splits))))]))

 (define (median pred lst)
   (let* ((len (length lst))
          (nlst (merge-sort pred lst))
          (m (if (even? len)
                 (/ (+ (list-ref nlst (/ len 2))
                       (list-ref nlst (- (/ len 2) 1))) 2)
                 (list-ref nlst (inexact->exact (floor (/ len 2)))))))
     (exact->inexact m)))

 (define (string-sort lst)
   (merge-sort (lambda (a b) (string<? (->string a) (->string b)))
               lst))

 )