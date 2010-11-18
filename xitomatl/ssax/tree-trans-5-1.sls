#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl ssax tree-trans (5 1))
  (export
    SRV:send-reply
    pre-post-order
    post-order
    foldts
    replace-range)
  (import
    (except (rnrs) error)
    (xitomatl include)
    (xitomatl ssax private-5-1 error))
  
  (define error (make-errorer "(xitomatl ssax tree-trans)"))
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "SXML-tree-trans.scm")
)
