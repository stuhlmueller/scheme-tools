#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl ssax private-5-1 look-for-str)
  (export
    MISCIO:find-string-from-port?
    find-string-from-port?)
  (import
    (rnrs)
    (xitomatl include)
    (xitomatl ssax private-5-1 misc))
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "look-for-str.scm")  
)
