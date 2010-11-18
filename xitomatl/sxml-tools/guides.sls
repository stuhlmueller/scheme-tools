#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl sxml-tools guides (2008 06 27))
  (export
    dgs:version
    dgs:fold
    dgs:find
    add-lp
    sxml-guide-flat
    sxml-guide
    xml-guide-flat)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (xitomatl include)
    (xitomatl ssax parsing)
    (xitomatl ssax private-5-1 output))

  (include/resolve ("xitomatl" "sxml-tools") "guides.scm")
)
