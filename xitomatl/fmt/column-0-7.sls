#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl fmt column (0 7))
  (export
    fmt-columns
    columnar
    fold-lines
    wrap-fold-words
    wrap-fold
    wrap-lines
    justify
    fmt-file
    line-numbers)
  (import
    (rnrs)
    (only (rnrs r5rs) inexact->exact quotient remainder)
    (only (srfi :1 lists) fold)
    (srfi :6 basic-string-ports)
    (only (srfi :13 strings) string-concatenate string-concatenate-reverse
                             string-index string-tokenize substring/shared)
    (srfi :23 error tricks)
    (xitomatl include)
    (xitomatl fmt base (0 7))
    (xitomatl fmt let-optionals*))
  
  (define read-line
    (case-lambda
      (() (read-line (current-input-port)))
      ((tip) (get-line tip))))

  (SRFI-23-error->R6RS "(library (xitomatl fmt column (0 7)))"  
   (include/resolve ("xitomatl" "fmt") "fmt-column.scm"))
)
