#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (rnrs mutable-pairs)
  (rnrs r5rs) 
  (srfi :26 cut)
  (only (xitomatl control) compose)
  (only (xitomatl common) with-input-from-string)
  (only (xitomatl strings) string-split)
  (xitomatl include)
  (xitomatl fmt)
  (xitomatl tests fmt test))

(define-syntax cond-expand (syntax-rules () ((_ . _) (begin))))

(include/resolve ("xitomatl" "tests" "fmt") "test-fmt.scm")
