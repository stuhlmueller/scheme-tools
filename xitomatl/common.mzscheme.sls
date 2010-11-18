#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl common)
  (export
    add1 sub1
    format printf fprintf pretty-print
    gensym
    time
    with-input-from-string with-output-to-string
    system
    ;; TODO: add to as needed/appropriate
    )
  (import
    (except (rnrs) current-input-port current-output-port)
    (only (scheme) add1 sub1 format printf fprintf gensym time
                   current-input-port current-output-port parameterize)
    (only (scheme pretty) pretty-print)
    (only (scheme system) system))

  (define (with-input-from-string str thunk)
    (parameterize ((current-input-port (open-string-input-port str)))
      (thunk)))
  
  (define (with-output-to-string thunk)
    (let-values (((sop get) (open-string-output-port)))
      (parameterize ((current-output-port sop))
        (thunk))
      (get)))
)
