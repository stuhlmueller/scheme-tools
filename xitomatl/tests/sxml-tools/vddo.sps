#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (xitomatl sxml-tools ddo-txpath)
  (xitomatl include)
  (xitomatl sxml-tools sxpathlib)
  (xitomatl tests sxml-tools xtest-harness)
  (xitomatl tests sxml-tools xtest-lib)
  (xitomatl ssax private-5-1 output)
  (rename (only (xitomatl common) pretty-print)
          (pretty-print pp)))

(include/resolve ("xitomatl" "tests" "sxml-tools") "vddo.scm")
