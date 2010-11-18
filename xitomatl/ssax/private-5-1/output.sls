#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl ssax private-5-1 output)
  (export
    cout cerr nl)
  (import
    (rnrs)
    (xitomatl include))  

  (include/resolve ("xitomatl" "ssax" "private-5-1") "output.scm")
)
