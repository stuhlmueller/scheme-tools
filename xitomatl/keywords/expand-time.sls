#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl keywords expand-time)
  (export
    parse-kw-formals
    process-options
    keywords-parser--define/kw)
  (import
    (rnrs)
    (only (xitomatl macro-utils)
          formals-ok?/raise)
    (only (xitomatl exceptions)
          syntax-violation/conditions)
    (xitomatl keywords expand-time process-options)
    (for (only (rnrs base)
               quote)
         (meta -1))
    (only (xitomatl keywords parser)
          keywords-parser--meta
          make-keyword-condition))

  (define (parse-kw-formals full-stx kw-formals)
    (let parse ((kwf kw-formals) (pos-ids '()))
      (syntax-case kwf ()
        (((kw-id . opts) ... . additional-id)
         (let ((pos-ids (reverse pos-ids)))
           (and (formals-ok?/raise
                 (append pos-ids #'(kw-id ... . additional-id))
                 full-stx)
                (cons* pos-ids
                       #'((kw-id . opts) ...)
                       (if (identifier? #'additional-id)
                         (list #'additional-id)
                         '())))))
        ((pos . r)
         (parse #'r (cons #'pos pos-ids))))))

  (define (missing-value--define/kw stx)
    (lambda (who kw)
      (syntax-violation/conditions who "keyword missing value" stx #F
                                   (make-keyword-condition kw))))

  (define (missing-keyword--define/kw stx)
    (lambda (who kw)
      (syntax-violation/conditions who "missing required keyword" stx #F
                                   (make-keyword-condition kw))))

  (define (predicate-false--dummy . _)
    (assert #F))

  (define (kw-stx=? x y)
    (syntax-case x (quote)
      ((quote id) (identifier? #'id)
       (eq? (syntax->datum #'id) y))
      (_ #F)))

  (define-syntax keywords-parser--define/kw
    (syntax-rules ()
      ((_  stx who . r)
       (let ((mv (missing-value--define/kw stx))
             (mk (missing-keyword--define/kw stx)))
         (keywords-parser--meta who kw-stx=?
          mv mk predicate-false--dummy
          . r)))))
)
