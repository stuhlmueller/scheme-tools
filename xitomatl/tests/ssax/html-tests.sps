#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (except (rnrs) assert)
  (xitomatl ssax html)
  (xitomatl ssax tree-trans)
  (xitomatl include)
  (srfi :78 lightweight-testing)
  (xitomatl ssax private-5-1 output)
  (xitomatl ssax private-5-1 misc))

(define-syntax assert
  (syntax-rules ()
    ((_ expr ...)
     (begin (check expr => #T) ...))))

(include/resolve ("xitomatl" "tests" "ssax") "vSXML-to-HTML.scm")

(check-report)
