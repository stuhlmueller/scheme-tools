#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl tests sxml-tools xtest-lib)
  (export
    xtest-filter
    xtest-ppw)
  (import
    (rnrs)
    (xitomatl include)
    (rename (only (xitomatl common) pretty-print)
            (pretty-print pp)))

  (include/resolve ("xitomatl" "tests" "sxml-tools") "xtest-lib.scm")
)
