#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl IU-match macro-helpers)
  (export
    find-ids/prevent-auto-recur
    check-ids/prevent-dups-across
    check-ids/dups-okay)
  (import
    (rnrs)
    (for (only (rnrs base) unquote) (meta -1)))
  
  (define (problem form-stx kw-name-stx msg subform)
    (syntax-violation #F msg
      (syntax-case form-stx () ((_ ctxt . rest) #`(#,kw-name-stx . rest)))
      subform))  
  
  (define (find-ids/prevent-auto-recur form-stx kw-name-stx pat)
    ;; Destructures a match pattern syntax to extract pattern variable identifiers.
    ;; Detects attempted uses of match's auto-recursion ability because it doesn't
    ;; make any sense for matching using a single pattern only.
    ;; Returns a list of the extracted syntax-object identifiers.
    (define-syntax self-recur
      (syntax-rules ()
        ((_ x) (find-ids/prevent-auto-recur form-stx kw-name-stx x))))
    (define (not-an-id x) 
      (problem form-stx kw-name-stx "not an identifier" x))
    (syntax-case pat (unquote)
      ((unquote id/recur)
       (if (identifier? #'id/recur)
         (list #'id/recur)
         (not-an-id #'id/recur)))
      (((unquote id/recur) . rest)
       (if (identifier? #'id/recur)
         (cons #'id/recur (self-recur #'rest))
         (not-an-id #'id/recur)))
      ((any . rest)
       (append (self-recur #'any) (self-recur #'rest)))
      (#((unquote id/recur) rest ...)
       (if (identifier? #'id/recur)
         (cons #'id/recur (self-recur #'(rest ...)))
         (not-an-id #'id/recur)))
      (#(any rest ...)
       (append (self-recur #'any) (self-recur #'(rest ...))))
      (atom '())))
  
  (define (check-ids/prevent-dups-across form-stx kw-name-stx pat*)
    ;; Extracts all pattern variable identifiers in the supplied pattern syntaxes and
    ;; prevents attempted uses of match's auto-recursion, using find-ids/prevent-auto-recur,
    ;; and prevents duplicate identifiers accross the patterns, while allowing 
    ;; duplicate identifiers in a single pattern.
    ;; Returns #T if all checks pass.
    (define pat*-ids 
      (map (lambda (pat) (find-ids/prevent-auto-recur form-stx kw-name-stx pat)) pat*))
    (if (null? pat*-ids)
      #T
      (let loop ((first (car pat*-ids)) (others (cdr pat*-ids)))
        (if (null? others)
          #T
          (begin
            (for-each 
              (lambda (fid)
                (for-each 
                  (lambda (other)
                    (for-each 
                      (lambda (oid) 
                        (when (bound-identifier=? fid oid)
                          (problem form-stx kw-name-stx "duplicate binding across patterns" fid))) 
                      other))
                  others))
              first)
            (loop (car others) (cdr others)))))))
  
  (define (check-ids/dups-okay form-stx kw-name-stx pat*)
    ;; Uses find-ids/prevent-auto-recur to prevent attempted use of match's 
    ;; auto-recursion, and allows duplicate pattern variable identifiers
    ;; across patterns.
    ;; Returns #T if the checks pass.
    (for-each 
      (lambda (pat) (find-ids/prevent-auto-recur form-stx kw-name-stx pat))
      pat*)
    #T)   
)
