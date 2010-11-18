#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (except (rnrs) assert)
  (xitomatl ssax tree-trans)
  (xitomatl include)
  (srfi :78 lightweight-testing)
  (xitomatl ssax private-5-1 output)
  (only (xitomatl common) pretty-print))

(define-syntax assert
  (syntax-rules ()
    ((_ expr ...)
     (begin (check expr => #T) ...))))

(define (pp x)
  (display "\nPretty Printed:\n")
  (pretty-print x)
  (newline))

(include/resolve ("xitomatl" "tests" "ssax") "vSXML-tree-trans.scm")

(check-report)
