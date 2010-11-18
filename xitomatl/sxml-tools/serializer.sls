#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl sxml-tools serializer (2008 06 27))
  (export
    srl:map-append
    srl:apply-string-append
    srl:assoc-cdr-string=
    srl:member-ci
    srl:mem-pred
    srl:char-nl
    srl:newline
    srl:select-kids
    srl:separate-list
    srl:clean-fragments
    srl:display-fragments-2nesting
    srl:split-name
    srl:atomic->string
    srl:empty-elem?
    srl:conventional-ns-prefixes
    srl:namespace-assoc-for-elem
    srl:ns-assoc-for-top
    srl:extract-original-prefix-binding
    srl:update-space-specifier
    srl:normalize-sequence
    srl:xml-char-escaped
    srl:string->cdata-section
    srl:escape-alist-char-data
    srl:escape-alist-att-value
    srl:escape-alist-html-att
    srl:string->escaped
    srl:string->char-data
    srl:string->att-value
    srl:string->html-att
    srl:shtml-entity->char-data
    srl:qname->string
    srl:attribute->str-lst
    srl:namespace-decl->str-lst
    srl:comment->str-lst
    srl:processing-instruction->str-lst
    srl:name->qname-components
    srl:construct-start-end-tags
    srl:node->nested-str-lst-recursive
    srl:display-node-out-recursive
    srl:make-xml-decl
    srl:top->nested-str-lst
    srl:display-top-out
    srl:sxml->string
    srl:display-sxml
    srl:parameterizable
    srl:sxml->xml
    srl:sxml->xml-noindent
    srl:sxml->html
    srl:sxml->html-noindent)
  (import
    (rnrs)
    (xitomatl include))

  (include/resolve ("xitomatl" "sxml-tools") "serializer.scm")
)
