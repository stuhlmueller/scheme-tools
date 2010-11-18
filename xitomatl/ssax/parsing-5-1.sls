#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

;; NOTE: The SXML created and understood by this library uses
;;       ^ instead of @ because @ is not a valid R6RS symbol.

(library (xitomatl ssax parsing (5 1))
  (export
    make-xml-token
    xml-token?
    xml-token-kind
    xml-token-head
    ssax:S-chars
    ssax:skip-S
    ssax:ncname-starting-char?
    ssax:read-NCName
    ssax:read-QName
    ssax:Prefix-XML
    name-compare
    ssax:largest-unres-name
    ssax:read-markup-token
    ssax:skip-pi
    ssax:read-pi-body-as-string
    ssax:skip-internal-dtd
    ssax:read-cdata-body
    ssax:read-char-ref
    ssax:predefined-parsed-entities
    ssax:handle-parsed-entity
    make-empty-attlist
    attlist-add
    attlist-null?
    attlist-remove-top
    attlist->alist
    attlist-fold
    ssax:read-attributes
    ssax:resolve-name
    ssax:uri-string->symbol
    ssax:complete-start-tag
    ssax:read-external-id
    ssax:scan-Misc
    ssax:read-char-data
    ssax:assert-token
    ssax:make-pi-parser
    ssax:make-elem-parser
    ssax:make-parser
    ssax:make-parser/positional-args
    ssax:define-labeled-arg-macro
    ssax:reverse-collect-str
    ssax:reverse-collect-str-drop-ws
    ssax:xml->sxml
    SSAX:XML->SXML)
  (import
    (except (rnrs) fold-right error)
    (xitomatl include)
    (except (srfi :13 strings) string-copy string->list string-titlecase
            string-upcase string-downcase string-hash string-for-each)
    (only (xitomatl control) begin0)
    (xitomatl ssax raise)
    (xitomatl ssax private-5-1 define-opt)
    (xitomatl ssax private-5-1 input-parse)
    (xitomatl ssax private-5-1 look-for-str)
    (xitomatl ssax private-5-1 output)
    (xitomatl ssax private-5-1 misc)
    (xitomatl ssax private-5-1 error)
    (xitomatl ssax private-5-1 util))
  
  (define error (make-errorer "(xitomatl ssax parsing)"))
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "SSAX.scm")  
)
