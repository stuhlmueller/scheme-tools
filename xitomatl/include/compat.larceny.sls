;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl include compat)
  (export
    search-paths stale-when (rename (read read-annotated)))
  (import
    (rnrs base)
    (only (rnrs io simple) read)
    (srfi :98 os-environment-variables)
    (only (xitomatl file-system paths) absolute-path? path-join)
    (primitives current-require-path))

  (define (search-paths)
    (let ((larceny-root (get-environment-variable "LARCENY_ROOT")))
      (map (lambda (crp)
             (if (absolute-path? crp)
               crp
               (path-join larceny-root crp)))
           (current-require-path))))

  (define-syntax stale-when
    (syntax-rules ()
      ((_ when-expr . r)
       (begin . r))))
)
