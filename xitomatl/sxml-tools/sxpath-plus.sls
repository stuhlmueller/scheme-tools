#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl sxml-tools sxpath-plus (2008 06 27))
  (export
    analyze-red1
    analyze-reduce
    analyze-1
    analyze-step
    analyze-path
    sxpath+)
  (import
    (rename (rnrs) (syntax->datum syntax-object->datum))
    (xitomatl include))

  (include/resolve ("xitomatl" "sxml-tools") "sxpath-plus.scm")
)
