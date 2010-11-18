#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl ssax private-5-1 to-html)
  (export
    SXML->HTML
    string->goodHTML
    enattr
    entag)
  (import
    (rnrs)
    (xitomatl include)
    (xitomatl ssax tree-trans)
    (xitomatl ssax private-5-1 output)
    (xitomatl ssax private-5-1 util))
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "SXML-to-HTML.scm")
)
