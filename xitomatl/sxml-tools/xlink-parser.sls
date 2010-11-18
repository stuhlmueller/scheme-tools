#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl sxml-tools xlink-parser (2008 06 27))
  (export
    xlink:namespace-uri
    xlink:linkbase-uri
    xlink:make-small-seed
    xlink:make-full-seed
    xlink:seed-mode
    xlink:seed-sxlink-arcs
    xlink:seed-sxpointer
    xlink:seed-stack
    xlink:seed-locators+resources
    xlink:seed-arcs
    xlink:seed-declared-labels
    xlink:add-simple
    xlink:add-extended
    xlink:sxpointer->childseq
    xlink:sxpointer4sibling
    xlink:make-locator-or-resource
    xlink:resource-label
    xlink:resource-data
    xlink:add-locator
    xlink:add-resource
    xlink:make-arc-info
    xlink:arc-info-from
    xlink:arc-info-to
    xlink:arc-info-linkbase
    xlink:arc-info-position
    xlink:arc-info-data
    xlink:add-arc
    xlink:add-default-arc
    xlink:add-declared-label
    xlink:all-labels-declared
    xlink:construct-xlink-values
    xlink:values-type
    xlink:values-href
    xlink:values-role
    xlink:values-arcrole
    xlink:values-title
    xlink:values-show
    xlink:values-actuate
    xlink:values-label
    xlink:values-from
    xlink:values-to
    xlink:read-attributes
    xlink:read-SXML-attributes
    xlink:check-helper
    xlink:check-type-show-actuate-constraints
    xlink:general-start
    xlink:general-end
    xlink:none-start
    xlink:none-end
    xlink:simple-start
    xlink:simple-end
    xlink:extended-start
    xlink:extended-end
    xlink:locator-start
    xlink:locator-end
    xlink:resource-start
    xlink:resource-end
    xlink:arc-start
    xlink:arc-end
    xlink:get-port-position
    xlink:parser-error
    xlink:branch-helper
    xlink:replace-branch
    xlink:append-branch
    xlink:get-uri
    xlink:set-uri-for-sxlink-arcs
    xlink:new-level-seed-handler
    xlink:finish-element-handler
    xlink:ending-action
    SXML->SXML+xlink
    SHTML->SHTML+xlink)
  (import
    (rnrs)
    (xitomatl include)
    (rename (except (srfi :13 strings) string-copy string->list string-titlecase
                    string-upcase string-downcase string-hash string-for-each)
            (string-index-right string-rindex))
    (xitomatl sxml-tools sxpathlib)
    (xitomatl ssax private-5-1 output)
    (xitomatl ssax private-5-1 util))

  (define (ar:resolve-uri-according-base . args)
    (assertion-violation 'ar:resolve-uri-according-base
      "not implemented because don't know where to get it"))

  (include/resolve ("xitomatl" "sxml-tools") "xlink-parser.scm")
)
