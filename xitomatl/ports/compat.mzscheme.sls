#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl ports compat)
  (export
    port-closed?)
  (import
    (only (scheme base) port-closed?))
)
