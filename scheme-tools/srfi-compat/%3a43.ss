#!r6rs

;; In order to be compatible with (rnrs), this does not export:
;; vector-map, vector-for-each, vector-fill!, vector->list, list->vector

(library

 (scheme-tools srfi-compat :43)

 (export make-vector
         reverse-list->vector
         reverse-vector->list
         vector
         vector-any
         vector-append
         vector-binary-search
         vector-concatenate
         vector-copy
         vector-copy!
         vector-count
         vector-empty?
         vector-every
         vector-fold
         vector-fold-right
         vector-index
         vector-index-right
         vector-length
         vector-map!
         vector-ref
         vector-reverse!
         vector-reverse-copy
         vector-reverse-copy!
         vector-set!
         vector-skip
         vector-skip-right
         vector-swap!
         vector-unfold
         vector-unfold-right
         vector=
         vector?)

 (import (srfi :43))

 )