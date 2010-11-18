#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl ssax private-5-1 input-parse)
  (export
    peek-next-char
    assert-curr-char
    skip-until
    skip-while
    input-parse:init-buffer
    next-token-old
    next-token
    next-token-of
    *read-line-breaks*
    read-text-line
    read-string)
  (import
    (rnrs)
    (xitomatl include)
    (xitomatl ssax private-5-1 define-opt)
    (xitomatl ssax raise)
    (xitomatl ssax private-5-1 misc)
    (except (srfi :13 strings) 
            string-copy string-for-each string->list
            string-upcase string-downcase string-titlecase string-hash))
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "input-parse.scm")
)
