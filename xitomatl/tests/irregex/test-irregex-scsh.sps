#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import 
  (rnrs)
  (xitomatl irregex)
  (xitomatl tests irregex test)
  (xitomatl include))

(include/resolve ("xitomatl" "tests" "irregex") "test-irregex-scsh.scm")

(test-exit 69)
