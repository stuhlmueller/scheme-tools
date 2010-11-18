#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl sxml-match void)
  (export
    (rename (make-void void)))
  (import
    (rnrs))
  
  (define-record-type void)
)
