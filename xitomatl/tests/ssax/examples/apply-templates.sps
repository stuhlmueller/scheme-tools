#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (rnrs r5rs)
  (xitomatl include)
  (xitomatl ssax html)
  (xitomatl ssax sxpath)
  (xitomatl ssax private-5-1 output)
  (xitomatl ssax private-5-1 misc)
  (rename (only (xitomatl common) pretty-print) (pretty-print pp)))

(include/resolve ("xitomatl" "tests" "ssax" "examples") "apply-templates.scm")
