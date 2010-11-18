#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl sxml-tools sxml-tools (2008 06 27))
  (export
    sxml:attr-list-node
    sxml:attr-as-list
    sxml:aux-list-node
    sxml:aux-as-list
    sxml:find-name-separator
    sxml:error
    sxml:empty-element?
    sxml:shallow-normalized?
    sxml:normalized?
    sxml:shallow-minimized?
    sxml:minimized?
    sxml:name
    sxml:element-name
    sxml:node-name
    sxml:ncname
    sxml:name->ns-id
    sxml:content
    sxml:text
    sxml:content-raw
    sxml:attr-list-u
    sxml:aux-list
    sxml:aux-list-u
    sxml:aux-node
    sxml:aux-nodes
    sxml:attr
    sxml:attr-from-list
    sxml:num-attr
    sxml:attr-u
    sxml:ns-list
    sxml:ns-id->nodes
    sxml:ns-id->uri
    sxml:ns-uri->nodes
    sxml:ns-uri->id
    sxml:ns-id
    sxml:ns-uri
    sxml:ns-prefix
    sxml:change-content!
    sxml:change-content
    sxml:change-attrlist
    sxml:change-attrlist!
    sxml:change-name!
    sxml:change-name
    sxml:add-attr
    sxml:add-attr!
    sxml:change-attr
    sxml:change-attr!
    sxml:set-attr
    sxml:set-attr!
    sxml:add-aux
    sxml:add-aux!
    sxml:squeeze!
    sxml:squeeze
    sxml:clean
    select-first-kid
    sxml:node-parent
    sxml:add-parents
    sxml:lookup
    sxml:attr->xml
    sxml:string->xml
    sxml:sxml->xml
    sxml:attr->html
    sxml:string->html
    sxml:non-terminated-html-tag?
    sxml:sxml->html)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (xitomatl include)
    (xitomatl sxml-tools sxpathlib)
    (xitomatl ssax private-5-1 output)
    (xitomatl ssax private-5-1 util))

  (include/resolve ("xitomatl" "sxml-tools") "sxml-tools.scm")
)
