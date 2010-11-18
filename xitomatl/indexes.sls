#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl indexes)
  (export
    iota
    enumerate)
  (import
    (rnrs)
    (only (xitomatl define) define/?)
    (only (xitomatl predicates) exact-non-negative-integer?)
    (xitomatl generics))
  
  (define (_iota n l)
    (if (= n -1)
      l
      (_iota (- n 1) (cons n l))))
  
  (define/? (iota (n exact-non-negative-integer?))
    (_iota (- n 1) '()))
  
  (define (_enumerate len)
    (_iota (- len 1) '()))
  
  (define-generic/temporal enumerate
    (((x list?)) (_enumerate (length x)))
    (((x vector?)) (_enumerate (vector-length x)))
    (((x string?)) (_enumerate (string-length x)))
    (((x bytevector?)) (_enumerate (bytevector-length x))))
  
)
