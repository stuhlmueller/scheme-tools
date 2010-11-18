#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl sxml-tools sxpathlib (2008 06 27))
  (export
    nodeset?
    as-nodeset
    sxml:element?
    ntype-names??
    ntype??
    ntype-namespace-id??
    sxml:complement
    node-eq?
    node-equal?
    node-pos
    sxml:filter
    take-until
    take-after
    map-union
    node-reverse
    node-trace
    select-kids
    node-self
    node-join
    node-reduce
    node-or
    node-closure
    sxml:node?
    sxml:attr-list
    sxml:attribute
    sxml:child
    sxml:parent
    node-parent
    sxml:child-nodes
    sxml:child-elements)
  (import
    (rnrs)
    (xitomatl include)
    (rename (except (srfi :13 strings) string-copy string->list string-titlecase
                    string-upcase string-downcase string-hash string-for-each)
            (string-index-right string-rindex))
    (xitomatl ssax private-5-1 misc)
    (xitomatl ssax private-5-1 output)
    (rename (only (xitomatl common) pretty-print)
            (pretty-print pp)))

  (include/resolve ("xitomatl" "sxml-tools") "sxpathlib.scm")
)
