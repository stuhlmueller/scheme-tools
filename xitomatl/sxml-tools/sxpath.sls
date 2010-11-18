#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl sxml-tools sxpath (2008 06 27))
  (export
    sxpath
    if-sxpath
    if-car-sxpath
    car-sxpath
    sxml:id-alist)
  (import
    (rnrs)
    (xitomatl include)
    (srfi :2 and-let*)
    (xitomatl sxml-tools sxml-tools)
    (xitomatl sxml-tools sxpathlib)
    (xitomatl sxml-tools sxpath-ext)
    (xitomatl sxml-tools txpath)
    (xitomatl ssax private-5-1 output))

  (include/resolve ("xitomatl" "sxml-tools") "sxpath.scm")
)
