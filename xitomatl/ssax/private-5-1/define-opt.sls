#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl ssax private-5-1 define-opt)
  (export
    define-opt)
  (import
    (except (rnrs) error)
    (xitomatl include)
    (xitomatl ssax private-5-1 error))
  
  (define error (make-errorer "(xitomatl ssax private-5-1 define-opt)"))
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "define-opt.scm")  
)
