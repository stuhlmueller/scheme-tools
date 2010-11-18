#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl strings compat)
  (export
    string-copy!)
  (import
    (rnrs base)
    (prefix (only (scheme base) string-copy!) mz:))
  
  (define (string-copy! src src-start dst dst-start k)
    (mz:string-copy! dst dst-start src src-start (+ src-start k)))
  
)
