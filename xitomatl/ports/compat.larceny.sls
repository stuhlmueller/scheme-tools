;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl ports compat)
  (export
    port-closed?)
  (import
    (rnrs base)
    (rnrs io ports)
    (only (xitomatl define) define/?))

  (define/? (port-closed? (p port?))
    (not (or (input-port? p) (output-port? p))))
)
