#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl sxml-tools txpath (2008 06 27))
  (export
    sxml:xpointer-runtime-error
    sxml:xpath-nodeset-filter
    sxml:arithmetic-eval
    sxml:core-last
    sxml:core-position
    sxml:core-count
    sxml:core-id
    sxml:core-local-name
    sxml:core-namespace-uri
    sxml:core-name
    sxml:core-string
    sxml:core-concat
    sxml:core-starts-with
    sxml:core-contains
    sxml:core-substring-before
    sxml:core-substring-after
    sxml:core-substring
    sxml:core-string-length
    sxml:core-normalize-space
    sxml:core-translate
    sxml:core-boolean
    sxml:core-not
    sxml:core-true
    sxml:core-false
    sxml:core-lang
    sxml:core-number
    sxml:core-sum
    sxml:core-floor
    sxml:core-ceiling
    sxml:core-round
    sxml:classic-params
    sxml:api-helper0
    sxml:classic-res
    sxml:api-helper
    sxml:xpath
    sxml:xpointer
    sxml:xpath-expr
    sxml:xpath+root+vars
    sxml:xpointer+root+vars
    sxml:xpath+root
    txpath
    sxml:api-index-helper
    sxml:xpath+index
    sxml:xpointer+index)
  (import
    (rnrs)
    (rnrs r5rs)
    (xitomatl include)
    (rename (except (srfi :13 strings) string-copy string->list string-titlecase
                    string-upcase string-downcase string-hash string-for-each)
            (string-index-right string-rindex))
    (xitomatl sxml-tools xpath-parser)
    (xitomatl sxml-tools sxml-tools)
    (xitomatl sxml-tools sxpath-ext)
    (xitomatl sxml-tools sxpathlib)
    (xitomatl ssax private-5-1 output)
    (xitomatl ssax private-5-1 util))

  (include/resolve ("xitomatl" "sxml-tools") "txpath.scm")
)
