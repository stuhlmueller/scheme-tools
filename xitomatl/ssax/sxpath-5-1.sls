#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl ssax sxpath (5 1))
  (export
    nodeset?
    node-typeof?
    node-eq?
    node-equal?
    node-pos
    (rename (filter node-filter))
    take-until
    take-after
    map-union
    node-reverse
    node-trace
    select-kids
    node-self
    node-join
    node-reduce
    node-or
    node-closure
    node-parent
    sxpath)
  (import
    (except (rnrs) error filter)
    (xitomatl include)
    (xitomatl ssax private-5-1 error)
    (xitomatl ssax private-5-1 output)
    (xitomatl ssax private-5-1 misc)
    (only (xitomatl common) pretty-print))
  
  (define error (make-errorer "(xitomatl ssax sxpath)"))
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "SXPath-old.scm")
)
