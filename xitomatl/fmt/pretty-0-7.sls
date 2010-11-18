#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl fmt pretty (0 7))
  (export
    fmt-shares
    fmt-set-shares!
    fmt-copy-shares
    copy-shares
    fmt-shared-write
    fmt-join/shares
    non-app?
    syntax-abbrevs
    pp-let
    indent-rules
    indent-prefix-rules
    indent-suffix-rules
    pp-indentation
    pp-with-indent
    pp-app
    proper-non-shared-list?
    pp-flat
    pp-pair
    pp-data-list
    pp-vector
    pp-object
    pretty
    pretty/unshared)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (rnrs r5rs) quotient)
    (only (srfi :1 lists) length+)
    (only (srfi :13 strings) string-prefix? string-suffix?)
    (xitomatl include)
    (xitomatl fmt base (0 7))
    (only (srfi :69 basic-hash-tables) hash-table-walk))

  (include/resolve ("xitomatl" "fmt") "fmt-pretty.scm")  
)
