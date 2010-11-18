#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl library-utils)
  (export
    library-name-without-version
    library-name-version
    library-name?
    library-name-symbol?
    library-version?
    library-name<?
    library-version<?)
  (import
    (rnrs)
    (only (xitomatl define) define/?)
    (only (xitomatl predicates) list-of? exact-non-negative-integer?
                                pairwise? symbol<?))
  
  (define/? (library-name-without-version (name library-name?))
    (filter symbol? name))

  (define/? (library-name-version (name library-name?))
    (let ((last (list-ref name (- (length name) 1))))
      (and (list? last)
           last)))
 
  (define (library-name? x)
    (and (pair? x)
         (library-name-symbol? (car x))
         (let loop ((x (cdr x)))
           (if (pair? x)
             (if (library-name-symbol? (car x))
               (loop (cdr x))
               (and (library-version? (car x))
                    (null? (cdr x))))
             (null? x)))))

  (define (library-name-symbol? x)
    (and (symbol? x)
         (positive? (string-length (symbol->string x)))))

  (define library-version? (list-of? exact-non-negative-integer?))

  (define library-name<?
    (pairwise?
     (letrec ((name<?
               (lambda (x y)
                 (if (pair? x)
                   (and (pair? y)
                        (if (symbol? (car x))
                          (and (symbol? (car y))
                               (or (symbol<? (car x) (car y))
                                   (and (symbol=? (car x) (car y))
                                        (name<? (cdr x) (cdr y)))))
                          (or (symbol? (car y))
                              (library-version<? (car x) (car y)))))
                   (pair? y)))))
       name<?)
     (lambda (x)
       (if (library-name? x)
         x
         (assertion-violation 'library-name<? "not a library name" x)))))

  (define library-version<?
    (pairwise?
     (letrec ((version<?
               (lambda (x y)
                 (if (pair? x)
                   (and (pair? y)
                        (or (< (car x) (car y))
                            (and (= (car x) (car y))
                                 (version<? (cdr x) (cdr y)))))
                   (pair? y)))))
       version<?)
     (lambda (x)
       (if (library-version? x)
         x
         (assertion-violation 'library-version<?
                              "not a library version" x)))))

)
