;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl include compat)
  (export
    search-paths stale-when read-annotated)
  (import
    (rnrs base)
    (only (ikarus) library-path stale-when read-annotated))

  (define (search-paths)
    (library-path))
)
