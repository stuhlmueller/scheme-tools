#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl keywords parser)
  (export
    keyword-condition?
    condition-keyword
    make-keyword-condition
    missing-value--default
    missing-keyword--default
    predicate-false--default
    not-given
    not-given?
    keywords-parser--meta)
  (import
    (rnrs)
    (only (xitomatl conditions)
          make-predicate-expression-condition)
    (for (only (xitomatl macro-utils)
               with-syntax*
               gen-temp)
         expand)
    (for (only (xitomatl indexes)
               enumerate)
         expand)
    (for (xitomatl keywords expand-time process-options)
         expand))

  (define-condition-type &keyword &condition
    make-keyword-condition keyword-condition?
    (keyword condition-keyword))

  (define (AV who msg kw . more)
    (raise
     (apply condition
            (make-assertion-violation)
            (make-who-condition who)
            (make-message-condition msg)
            (make-keyword-condition kw)
            more)))

  (define (missing-value--default who kw)
    (AV who "keyword missing value" kw))

  (define (missing-keyword--default who kw)
    (AV who "missing required keyword" kw))

  (define (predicate-false--default who kw pred-expr value)
    (AV who "keyword predicate false" kw
        (make-predicate-expression-condition pred-expr)
        (make-irritants-condition (list value))))

  (define not-given (list #T))  ;; unique object
  (define (not-given? x) (eq? x not-given))

  (define-syntax keywords-parser--meta
    (lambda (stx)
      (define (gen-stx who kw=? missing-value missing-keyword predicate-false
                       kw-values process-input-list car-id cdr-id additional)
        (lambda (kw-spec index)
          (with-syntax (((kw-id . _) kw-spec)
                        (v (gen-temp)))
            (define (gen-clause/val)
              #`((#,kw=? #,car-id 'kw-id)
                 (if (pair? #,cdr-id)
                   (vector-set! #,kw-values #,index (car #,cdr-id))
                   (#,missing-value '#,who 'kw-id))
                 (#,process-input-list (cdr #,cdr-id) #,additional)))
            (define (gen-test/val true false)
              #`(let ((v (vector-ref #,kw-values #,index)))
                  (if (not-given? v) #,false #,true)))
            (define (gen-pred/val pred)
              #`(if (#,pred v)
                  v
                  (#,predicate-false '#,who 'kw-id '#,pred v)))
            (define (gen-missing)
              #`(#,missing-keyword '#,who 'kw-id))
            (define (gen-clause/bool)
              #`((#,kw=? #,car-id 'kw-id)
                 (vector-set! #,kw-values #,index #T)
                 (#,process-input-list #,cdr-id #,additional)))
            (define (gen-test/bool)
              #`(not (not-given? (vector-ref #,kw-values #,index))))
            (let-values (((default predicate boolean) (process-options stx kw-spec)))
              (cond ((and default predicate)
                     (list (gen-clause/val)
                           (gen-test/val (gen-pred/val predicate) default)))
                    (default
                     (list (gen-clause/val)
                           (gen-test/val #'v default)))
                    (predicate
                     (list (gen-clause/val)
                           (gen-test/val (gen-pred/val predicate) (gen-missing))))
                    (boolean
                     (list (gen-clause/bool)
                           (gen-test/bool)))
                    (else
                     (list (gen-clause/val)
                           (gen-test/val #'v (gen-missing)))))))))
      (syntax-case stx ()
        ((_ who kw=? missing-value missing-keyword predicate-false
            (kw-id options ...) ...)
         (for-all identifier?
                  #'(kw=? missing-value missing-keyword predicate-false kw-id ...))
         (with-syntax* ((num (length #'(kw-id ...)))
                        ((kw-values process-input-list car-id cdr-id additional)
                         (generate-temporaries '(1 2 3 4 5)))
                        (((cond-clause value-expr) ...)
                         (map (gen-stx #'who #'kw=?
                               #'missing-value #'missing-keyword #'predicate-false
                               #'kw-values #'process-input-list
                               #'car-id #'cdr-id #'additional)
                              #'((kw-id options ...) ...)
                              (enumerate #'(kw-id ...)))))
           #'(lambda (input-list)
               (let ((kw-values (make-vector num not-given)))
                 (let process-input-list ((l input-list) (additional '()))
                   (cond ((pair? l)
                          (let ((car-id (car l)) (cdr-id (cdr l)))
                            (cond cond-clause ...
                                  (else (process-input-list cdr-id
                                         (cons car-id additional))))))
                         ((null? l)
                          (letrec* ((kw-id value-expr) ...)
                            (values kw-id ... (reverse additional))))
                         (else
                          (assertion-violation 'who "not a proper list"
                                               input-list)))))))))))
)
