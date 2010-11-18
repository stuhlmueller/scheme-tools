;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl lists compat)
  (export
    make-list last-pair)
  (import
    (only (xitomatl define) define/?)
    (only (xitomatl predicates) exact-non-negative-integer?)
    (prefix (primitives make-list) larceny:)
    (primitives last-pair))

  (define/? make-list
    (case-lambda/?
      ((n) (make-list n #F))
      (((n exact-non-negative-integer?) v)
       (larceny:make-list n v))))
)
