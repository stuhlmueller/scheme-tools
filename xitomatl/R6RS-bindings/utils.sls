#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl R6RS-bindings utils)
  (export
    all-libraries-names 
    names-of)
  (import
    (rnrs)
    (rnrs eval)
    (rnrs r5rs)
    (only (xitomatl define) define/AV)
    (only (xitomatl exceptions) catch)
    (only (xitomatl library-utils) library-name<? library-name?)
    (xitomatl R6RS-bindings spec))
  
  (define all-libraries-names
    (list-sort library-name<?
               (filter library-name? (map car spec)) #| only (rnrs ---) libraries |# ))
  
  (define/AV names-of
    ;; This is a hack, and that's all I use it for.
    (case-lambda
      ((libname)
       (names-of libname 'all))
      ((libname type)
       (let ((names (cdr (or (assoc libname spec)
                             (AV "not in spec" libname))))
             (env (cond
                    ((library-name? libname) 
                     (environment libname))
                    ((eq? libname 'null-environment)
                     (null-environment 5))
                    ((eq? libname 'scheme-report-environment)
                     (scheme-report-environment 5))
                    (else (AV "invalid library name" libname)))))
         (case type
           ;; NOTE: It's possible a binding that is an identifier-syntax will
           ;; be considered a variable and not syntax by the below, but I don't
           ;; think there will ever be any of those in the (rnrs ---) libraries.
           ((syntaxes)
            (filter (lambda (x)
                      (catch ex ((else #T))
                        (eval x env)
                        #F))
                    names))
           ((variables)
            (filter (lambda (x)
                      (catch ex ((else #F))
                        (eval x env)
                        #T))
                    names))
           ((procedures)
            (filter (lambda (x)
                      (catch ex ((else #F))
                        (procedure? (eval x env))))
                    names))
           ((all) names)
           (else (AV "invalid mode" type)))))))
)
