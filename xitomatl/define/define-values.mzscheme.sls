#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl define define-values)
  (export
    (rename (my:define-values define-values)))
  (import
    (rnrs)
    (only (scheme base) define-values))
  
  (define-syntax my:define-values
    (lambda (stx)
      (syntax-case stx ()
        ((_ (id* ... . rid) expr)
         (identifier? #'rid)
         #'(define-values (id* ... rid)
             (let-values (((id* ... . rid) expr))
               (values id* ... rid))))
        ((_ . rest)
         #'(define-values . rest)))))
  
)
