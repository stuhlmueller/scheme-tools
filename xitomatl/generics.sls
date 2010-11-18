#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl generics)
  (export
    make-generic define-generic/temporal
    reconfigure/temporal)
  (import
    (rnrs)
    (for (only (xitomatl macro-utils) identifier-append) expand)
    (only (xitomatl define) define-values define/?))

  ;; Generics are procedures which delegate to some underlying procedure
  ;; determined by the arguments to the generic.  A per-generic association of
  ;; predicates to underlying-procedures is used to determine and find what
  ;; underlying to use.  No O.O.P. is involved, though it could be done on top.
  ;; Variable numbers of arguments ("rest arguments" lists) are fully supported.
  ;; Specializations are added at run-time, so new ones can be added by parties
  ;; unknown to the creator of a generic.  Precedence is according to the
  ;; temporal order specializations are added, which ensures the specializations
  ;; added by the creator always have precedence.  Performance will lessen the
  ;; more arguments a specialization has and will depend on how many other
  ;; specializations precede it.  Being fully run-time dynamic with a single
  ;; simple way of reconfiguring the specializations of a generic, the design is
  ;; open to possible future abilities such as removing specializations or
  ;; reordering their precedence.

  ;; For reconfigure/temporal and reconfigure/reverse-temporal, `preds' must be
  ;; of the type <argument-predicates> described in the below comment about
  ;; `specializations'.  That is, `preds' must be: a possibly empty list of
  ;; one-argument predicates which return true or #F, or an improper list of
  ;; predicates of the type just described but with the final cdr being an
  ;; any-number-of-arguments predicate which returns true or #F, or a predicate
  ;; of the type just described for the final cdr of the improper list case.
  ;; This matches the <formals> specification of a procedure's arguments: (args
  ;; ...) or (arg args ... . rest) or rest.  `proc' is the underlying procedure
  ;; delegated to for the arguments case specified by `preds'.  The number of
  ;; arguments `proc' accepts must match those specified by `preds'.  That is,
  ;; `proc' must accept as many arguments as there are one-argument predicates
  ;; in `preds', and if there is an any-number-of-arguments "rest arguments"
  ;; predicate in/as `preds', then `proc' must accept the additional, possibly
  ;; variable, number of "rest arguments" that the any-number-of-arguments
  ;; predicate returned true for.

  (define/? (reconfigure/temporal specializations
                                  (preds valid-predicates-specification?)
                                  (proc procedure?))
    (append specializations
            (list (list preds proc))))

  #;(define/? (reconfigure/reverse-temporal specializations
                                          (preds valid-predicates-specification?)
                                          (proc procedure?))
    (cons (list preds proc) specializations))

  #|(define (reconfigure/type-domain ---)
    ---)|#

  (define (valid-predicates-specification? x)
    (cond ((pair? x) (and (procedure? (car x))
                          (valid-predicates-specification? (cdr x))))
          ((null? x))
          ((procedure? x))
          (else #F)))

  (define (valid-specializations? x)
    (cond ((pair? x) (and (valid-predicates-specification? (caar x))
                          (procedure? (cadar x))
                          (valid-specializations? (cdr x))))
          ((null? x))
          (else #F)))

  (define (symbol-or-string? x) (or (symbol? x) (string? x)))

  (define/? make-generic
    (case-lambda/?
      (()
       (make-generic "a generic" "a generic specializer"))
      (((gwho symbol-or-string?) (swho symbol-or-string?))
       ;; specializations ::= (<specialization> ...)
       ;; <specialization> ::= (<argument-predicates> <specialized-procedure>
       ;;                       <supplemental> ...)
       ;; <argument-predicates> ::= (<predicate> ...)
       ;;                         | (<predicate> <predicate> ... . <rest-args-predicate>)
       ;;                         | <rest-args-predicate>
       ;; <predicate> ::= One-argument function which returns true or #F.
       ;; <rest-args-predicate> ::= Any-number-of-arguments function
       ;;                            which returns true or #F.
       ;; <specialized-procedure> ::= Procedure with arity matching <argument-predicates>
       ;;                             which returns any number and type of values
       ;; <supplemental> ::= Any value. Possibly used by reconfigure.
       (let ((specializations '()))
         (values
           ;; The generic
           (lambda args
             (let ((proc
                    (let next-spec ((specs specializations))
                      (cond
                        ((pair? specs)
                         (let next-pred ((preds (caar specs)) (test-args args))
                           (cond
                             ((pair? preds)
                              (if (and (pair? test-args)
                                       ((car preds) (car test-args)))
                                (next-pred (cdr preds) (cdr test-args))
                                (next-spec (cdr specs))))
                             ((null? preds)
                              (if (null? test-args)
                                (cadar specs)  ;; found the right specialized procedure
                                (next-spec (cdr specs))))
                             ((procedure? preds)  ;; predicate for rest args
                              (if (apply preds test-args)
                                (cadar specs)  ;; found the right specialized procedure
                                (next-spec (cdr specs))))
                             (else (assert #F)))))
                        ((null? specs)
                         (apply assertion-violation gwho "no specialization" args))
                        (else (assert #F))))))
               (apply proc args)))
           ;; Its specializations parameter (SRFI-39-compatible).
           (case-lambda
             (() specializations)
             ((x) (if (valid-specializations? x)
                    (set! specializations x)
                    (assertion-violation swho "invalid specializations value" x)))))))))

  (define (make-specializer reconfigure specializations)
    (lambda args
      (specializations (apply reconfigure (specializations) args))))

  (define-syntax define-generic--meta
    (lambda (stx)
      (syntax-case stx ()
        ((_ name reconfig (sargs ...) ...)
         (identifier? #'name)
         (with-syntax ((specialize! (identifier-append #'name #'name "-specialize!")))
           #'(define-values (name specialize!)
               (let-values (((g specs) (make-generic 'name 'specialize!)))
                 (let ((sg (make-specializer reconfig specs)))
                   (sg sargs ...)
                   ...
                   (values g sg)))))))))

  (define-syntax define-generic/temporal
    ;; (define-generic/temporal <identifier> <specialization-clause> ...)
    ;;
    ;; <specialization-clause> ::= (<predicate-formals> . <body>)
    ;; <predicate-formals> ::= ((<identifier> <predicate>) ...)
    ;;                       | ((<identifier> <predicate>)
    ;;                          (<identifier> <predicate>) ...
    ;;                          . #(<identifier> <rest-predicate>))
    ;;                       | #(<identifier> <rest-predicate>)
    ;; <predicate> ::= Expression which evaluates to a one-argument
    ;;                 function that returns true or #F.
    ;; <rest-predicate> ::= Expression which evaluates to an
    ;;                      any-number-of-arguments function that
    ;;                      returns true or #F.
    (lambda (stx)
      (syntax-case stx ()
        ((_ name (pred-frmls . b) ...)
         (identifier? #'name)
         (with-syntax ((((preds frmls) ...)
                        (map (lambda (pf)
                               (syntax-case pf ()
                                 (((a p) ...)
                                  #'((list p ...) (a ...)))
                                 (((a p) ... . #(ar pr))
                                  #'((cons* p ... pr) (a ... . ar)))))
                             #'(pred-frmls ...))))
           #'(define-generic--meta name reconfigure/temporal
               (preds (lambda frmls . b)) ...))))))
)
