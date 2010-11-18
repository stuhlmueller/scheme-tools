#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

;; This library is a combination of xpath-context and xlink because
;; otherwise separately they would create an import circle.

(library (xitomatl sxml-tools xpath-context--xlink (2008 06 27))
  (export
    ;; from xpath-context
    sxml:context-u?
    sxml:context?
    sxml:context->node-u
    sxml:context->ancestors-u
    sxml:context->content-u
    sxml:context->node
    sxml:context->ancestors
    sxml:context->content
    draft:contextset->nodeset
    draft:make-context
    draft:smart-make-context
    draft:siblings->context-set
    draft:na+
    draft:na-minus
    draft:na-minus-nneg
    draft:na-max
    draft:na-min
    draft:na>
    draft:na>=
    draft:list-head
    draft:list-last
    draft:make-list
    draft:signal-semantic-error
    draft:top?
    draft:remove-eq-duplicates
    draft:reach-root
    draft:recover-contextset
    draft:find-proper-context
    draft:ancestor
    draft:ancestor-or-self
    draft:attribute
    draft:child
    draft:descendant
    draft:descendant-or-self
    draft:following
    draft:following-sibling
    draft:namespace
    draft:parent
    draft:preceding
    draft:preceding-sibling
    draft:self
    draft:core-last
    draft:core-position
    draft:core-count
    draft:core-id
    draft:core-local-name
    draft:core-namespace-uri
    draft:core-name
    draft:core-string
    draft:core-concat
    draft:core-starts-with
    draft:core-contains
    draft:core-substring-before
    draft:core-substring-after
    draft:core-substring
    draft:core-string-length
    draft:core-normalize-space
    draft:core-translate
    draft:core-boolean
    draft:core-not
    draft:core-true
    draft:core-false
    draft:core-lang
    draft:core-number
    draft:core-sum
    draft:core-floor
    draft:core-ceiling
    draft:core-round
    draft:ast-axis-specifier
    draft:ast-node-test
    draft:ast-location-path
    draft:ast-absolute-location-path
    draft:ast-relative-location-path
    draft:ast-step
    draft:ast-step-list
    draft:ast-predicate
    draft:ast-predicate-list
    draft:ast-expr
    draft:ast-or-expr
    draft:ast-and-expr
    draft:ast-equality-expr
    draft:ast-relational-expr
    draft:ast-additive-expr
    draft:ast-multiplicative-expr
    draft:ast-union-expr
    draft:ast-path-expr
    draft:ast-filter-expr
    draft:ast-variable-reference
    draft:ast-literal
    draft:ast-number
    draft:ast-function-call
    draft:ast-function-arguments
    draft:ast-xpointer
    draft:ast-child-seq
    draft:ast-number-list
    draft:ast-full-xptr
    draft:arglist->ns+na
    draft:api-helper
    draft:xpath
    draft:xpointer
    draft:xpath-expr
    draft:sxpath
    txpath-with-context
    txpath/c
    sxpath-with-context
    sxpath/c
    ;; from xlink
    xlink:ntype??
    xlink:elem-extended?
    xlink:elem-simple?
    xlink:elem-locator?
    xlink:elem-resource?
    xlink:elem-arc?
    xlink:elem-title?
    xlink:set-uri
    xlink:id-index
    xlink:arcs-declared-here
    xlink:arcs-embedded?
    xlink:arcs-outgoing
    xlink:api-error
    xlink:parser
    xlink:get-document-by-uri
    xlink:arcs-uris
    xlink:arcs-linkbase-uris
    xlink:uris
    xlink:remove-equal-duplicates
    xlink:find-doc
    xlink:referenced-uris
    xlink:referenced-linkbase-uris
    xlink:add-documents-helper
    xlink:add-linkbases-recursively
    xlink:add-documents-recursively
    xlink:get-documents-with-params
    xlink:get-documents+linkbases
    xlink:unite-duplicate-keys-in-alist
    xlink:docs-exchange-arcs
    xlink:embed-arcs-into-document
    xlink:arcs-embedded
    xlink:parameterized-load-with-respect-documents
    xlink:get-docs-with-respect-to-loaded
    xlink:load-linked-docs-with-params
    xlink:documents
    xlink:documents-embed
    sxml:document
    xlink:arc?
    xlink:docs-variable
    xlink:add-docs-to-vars
    xlink:node-embedded-arcs
    xlink:node-arcs-on-top
    xlink:node-arcs
    xlink:traverse-arcs
    xlink:axis-arc
    xlink:axis-traverse
    xlink:axis-traverse-arc)
  (import
    (rnrs)
    (rnrs r5rs)
    (xitomatl include)
    (rename (except (srfi :13 strings) string-copy string->list string-titlecase
                    string-upcase string-downcase string-hash string-for-each)
            (string-index-right string-rindex))
    (srfi :2 and-let*)
    (xitomatl htmlprag)
    (xitomatl sxml-tools sxml-tools)
    (xitomatl sxml-tools sxpathlib)
    (xitomatl sxml-tools xpath-parser)
    (xitomatl sxml-tools sxpath-ext)
    (xitomatl sxml-tools txpath)
    (xitomatl sxml-tools xpath-ast)
    (xitomatl sxml-tools xlink-parser)
    (xitomatl ssax parsing)
    (xitomatl ssax multi-parser)
    (xitomatl ssax private-5-1 util)
    (xitomatl ssax private-5-1 output))

  (include/resolve ("xitomatl" "sxml-tools") "xpath-context.scm")

  (define (open-input-resource . args)
    (error 'open-input-resource "currently not implemented"))
  
  (define (ar:resource-type . args)
    (error 'ar:resource-type "currently not implemented"))
  
  (include/resolve ("xitomatl" "sxml-tools") "xlink.scm")
)
