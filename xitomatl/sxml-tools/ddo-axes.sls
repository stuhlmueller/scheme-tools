#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl sxml-tools ddo-axes (2008 06 27))
  (export
    ddo:list-last
    ddo:attr-child
    ddo:attrs-and-values
    ddo:discard-attributes
    ddo:ancestor
    ddo:ancestor-or-self
    ddo:attribute
    ddo:child
    ddo:descendant
    ddo:descendant-or-self
    ddo:following
    ddo:following-sibling
    ddo:namespace
    ddo:parent
    ddo:preceding
    ddo:preceding-sibling
    ddo:self
    ddo:following-single-level
    ddo:following-sibling-single-level
    ddo:parent-single-level
    ddo:preceding-single-level
    ddo:preceding-sibling-single-level
    ddo:ancestor-pos
    ddo:ancestor-or-self-pos
    ddo:child-pos
    ddo:descendant-pos
    ddo:descendant-or-self-pos
    ddo:following-sibling-pos
    ddo:parent-pos
    ddo:preceding-sibling-pos
    ddo:following-single-level-pos
    ddo:following-sibling-single-level-pos
    ddo:parent-single-level-pos
    ddo:preceding-single-level-pos
    ddo:preceding-sibling-single-level-pos)
  (import
    (rnrs)
    (xitomatl include)
    (xitomatl sxml-tools sxpathlib)
    (xitomatl sxml-tools xpath-context--xlink))

  (include/resolve ("xitomatl" "sxml-tools") "ddo-axes.scm")
)
