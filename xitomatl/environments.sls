#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl environments)
  (export
    environment
    environment?
    environment-symbols)
  (import
    (only (rnrs eval) environment)
    (xitomatl environments compat))
)
