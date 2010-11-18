#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl sxml-tools ddo-txpath (2008 06 27))
  (export
    ddo:or
    ddo:foldr
    ddo:type-nodeset
    ddo:type-number
    ddo:type-string
    ddo:type-boolean
    ddo:type-any
    ddo:nset-contained?
    ddo:nset-equal?
    ddo:pos-result-forward?
    ddo:pos-result->nodeset
    ddo:location-step-pos
    ddo:location-step-non-intersect
    ddo:location-step-non-pos
    ddo:filter-expr-general
    ddo:filter-expr-non-pos
    ddo:filter-expr-special-predicate
    ddo:all-contexts-in-doc
    ddo:unite-2-contextsets
    ddo:unite-multiple-context-sets
    ddo:list-tail
    ddo:list-head
    ddo:list-ref
    ddo:check-ast-position?
    ddo:check4ast-number
    ddo:check-special-predicate
    ddo:check-ast-desc-os?
    ddo:rewrite-step*
    ddo:generate-pred-id
    ddo:get-pred-value
    ddo:get-pred-value-pos
    ddo:get-abs-lpath-value
    ddo:construct-pred-values
    ddo:construct-pred-values-pos
    ddo:vector-copy-set
    ddo:add-vector-to-var-binding
    ddo:charlst->branch
    ddo:add-var-to-tree
    ddo:var-binding->tree
    ddo:get-var-value-from-tree
    ddo:ast-axis-specifier
    ddo:ast-location-path
    ddo:ast-absolute-location-path
    ddo:ast-relative-location-path
    ddo:ast-step
    ddo:ast-step-list
    ddo:ast-predicate
    ddo:ast-predicate-list
    ddo:ast-expr
    ddo:apply-ast-procedure
    ddo:ast-or-expr
    ddo:ast-and-expr
    ddo:ast-equality-expr
    ddo:ast-relational-expr
    ddo:ast-additive-expr
    ddo:ast-multiplicative-expr
    ddo:ast-union-expr
    ddo:ast-path-expr
    ddo:ast-filter-expr
    ddo:ast-variable-reference
    ddo:ast-literal
    ddo:ast-number
    ddo:ast-function-call
    ddo:ast-function-arguments
    ddo:api-helper
    ddo:txpath
    ddo:xpath-expr
    ddo:sxpath)
  (import
    (rnrs)
    (rnrs r5rs)
    (xitomatl include)
    (only (xitomatl common) gensym)
    (srfi :2 and-let*)
    (xitomatl sxml-tools sxpath-ext)
    (xitomatl sxml-tools sxpathlib)
    (xitomatl sxml-tools txpath)
    (xitomatl sxml-tools xpath-context--xlink)
    (xitomatl sxml-tools xpath-ast)
    (xitomatl sxml-tools ddo-axes))

  (include/resolve ("xitomatl" "sxml-tools") "ddo-txpath.scm")
)
