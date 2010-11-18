#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (xitomatl sxml-tools sxpathlib)
  (xitomatl include)
  (xitomatl sxml-tools sxpath)
  (xitomatl sxml-tools sxpath-ext)
  (xitomatl tests sxml-tools define-macro)
  (xitomatl ssax private-5-1 output))

(define (myenv:error . args)
  (apply assertion-violation 'sxp:error "test failed" args))

(include/resolve ("xitomatl" "tests" "sxml-tools") "vsxpathlib.scm")
