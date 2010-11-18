#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl box)
  (export
    box box? make-box box-value box-value-set!)
  (import
    (rnrs))
  
  (define-record-type box
    (fields (mutable value)))
)
