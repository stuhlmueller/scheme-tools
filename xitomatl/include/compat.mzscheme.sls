#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl include compat)
  (export
    search-paths stale-when (rename (read read-annotated)))
  (import
    (rnrs base)
    (only (rnrs io simple) read)
    (only (scheme base) current-library-collection-paths path->string)
    (only (scheme mpair) list->mlist))

  (define (search-paths)
    (map path->string 
         (list->mlist (current-library-collection-paths))))

  (define-syntax stale-when
    (syntax-rules ()
      ((_ when-expr . r)
       (begin . r))))
)
