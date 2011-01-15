#!r6rs

;; In order to be compatible with (rnrs), this does not export:
;; string-hash string-ci-hash

(library

 (scheme-tools srfi-compat :69)

 (export
  ;; Type constructors and predicate
  make-hash-table hash-table? alist->hash-table

  ;; Reflective queries
  hash-table-equivalence-function hash-table-hash-function

  ;; Dealing with single elements
  hash-table-ref hash-table-ref/default hash-table-set!
  hash-table-delete! hash-table-exists?
  hash-table-update! hash-table-update!/default

  ;; Dealing with the whole contents
  hash-table-size hash-table-keys hash-table-values hash-table-walk
  hash-table-fold hash-table->alist hash-table-copy hash-table-merge!

  ;; Hashing
  hash hash-by-identity)

 (import (srfi :69))
 
 )