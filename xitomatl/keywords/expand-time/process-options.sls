#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

;; This library exists to avoid a circular dependency.

(library (xitomatl keywords expand-time process-options)
  (export
    process-options)
  (import
    (rnrs)
    (only (xitomatl macro-utils)
          identifier?/name=?))

  (define (process-options full-stx kw-spec)
    (let loop ((options (with-syntax (((_ . opts) kw-spec)) #'opts))
               (default #F) (predicate #F) (boolean #F))
      (syntax-case options ()
        ((:default expr . rest)
         (and (identifier?/name=? #':default ':default)
              (not default))
         (loop #'rest #'expr predicate boolean))
        ((:predicate expr . rest)
         (and (identifier?/name=? #':predicate ':predicate)
              (not predicate))
         (loop #'rest default #'expr boolean))
        ((:boolean . rest)
         (and (identifier?/name=? #':boolean ':boolean)
              (not boolean))
         (loop #'rest default predicate #T))
        (()
         (not (and boolean (or default predicate)))
         (values default predicate boolean))
        (_
         (syntax-violation #F "invalid options for keyword" full-stx kw-spec)))))
)
