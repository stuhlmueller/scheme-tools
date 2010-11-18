#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (xitomatl include)
  (xitomatl fmt base)
  (xitomatl fmt c)
  (xitomatl tests fmt test))

(define-syntax cond-expand (syntax-rules () ((_ . _) (begin))))

(include/resolve ("xitomatl" "tests" "fmt") "test-fmt-c.scm")
