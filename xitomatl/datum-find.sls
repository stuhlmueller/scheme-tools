#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl datum-find)
  (export
    datum-find-enumerator
    datum-find
    datum-find->list)
  (import
    (rnrs)
    (only (xitomatl define) define/?/AV define/?)
    (only (xitomatl file-system base) directory-walk-enumerator)
    (only (xitomatl file-system paths) path? path-join path=?)
    (only (xitomatl ports) textual-input-port?)
    (only (xitomatl enumerators) input-port-enumerator)
    (only (xitomatl exceptions) catch warning)
    (only (xitomatl alists) assoc-update))
  
  (define/?/AV datum-find-enumerator 
    (case-lambda/?
      ((pred)
       (datum-find-enumerator pred #F))
      (((pred procedure?) want-warn)       
       (lambda (start proc seeds)
         (cond
           ((path? start)
            (find/dir-walk pred want-warn start proc seeds))
           ((textual-input-port? start)
            (find/port pred want-warn start proc seeds))
           (else 
            (AV "invalid start argument" start)))))))

  (define (find/port pred want-warn port proc seeds)
    ((input-port-enumerator (lambda (p)
                              (catch ex (((lexical-violation? ex)
                                          (when want-warn (warn ex port))
                                          (eof-object)))
                                (get-datum p))))
     port
     (lambda (datum . seeds)
       (let recur ((ds (list datum)) (seeds seeds))
         (if (null? ds)
           (apply values #T seeds)
           (let ((d (car ds)) (r (cdr ds)))
             (let-values (((c s)
                           (if (pred d)
                             (let-values (((continue . next-seeds) 
                                           (apply proc d seeds)))
                               (values continue next-seeds))
                             (values #T seeds))))
               (if c
                 (let destruct ((d d))
                   (cond 
                     ((pair? d)  ;; handle as improper list
                      (let loop ((h (car d)) (t (cdr d)) (a '()))
                        (cond ((pair? t)
                               (loop (car t) (cdr t) (cons h a)))
                              ((null? t)
                               (recur (apply cons* (reverse (cons* r h a))) s))
                              (else
                               (recur (apply cons* (reverse (cons* r t h a))) s)))))
                     ((vector? d)
                      (destruct (vector->list d)))
                     (else
                      (recur r s))))  
                 (apply values #F s)))))))
     seeds))
  
  (define (find/dir-walk pred want-warn start-path proc seeds)
    ((directory-walk-enumerator)
     start-path
     (lambda (path dirs files syms . seeds)
       (let next-file ((l files) (seeds seeds))
         (cond
           ((null? l)
            (apply values dirs seeds))
           (else
            (let* ((f (path-join path (car l)))
                   (fip (catch ex (((i/o-filename-error? ex)
                                    (when want-warn (warn ex f))
                                    #F))
                          (open-input-file f))))
              (if fip
                (let-values (((continue next-seeds)
                              (find/port pred want-warn fip 
                                         (lambda (datum _ seeds) 
                                           (let-values (((c . s)
                                                         (apply proc datum f seeds)))
                                             (values c c s)))
                                         (list #T seeds))))
                  (close-port fip)
                  (if continue
                    (next-file (cdr l) next-seeds)
                    (apply values #F next-seeds)))
                (next-file (cdr l) seeds)))))))
     seeds))
  
  (define (warn ex . irrts)
    (apply warning  ;; does raise-continuable
           'datum-find-enumerator
           "Exception raised from datum finding"
           (if (condition? ex) (simple-conditions ex) ex)
           irrts)
    #|Continuing not working with PLT's
      broken R6RS exceptions implementation.|#)
  
  (define/? datum-find 
    (case-lambda
      ((pred)
       (datum-find pred #F))
      ((pred want-warn) 
       (lambda/? ((proc procedure?) start)
         ((datum-find-enumerator pred want-warn) 
          start
          (case-lambda
            ((d f) (proc d f) #T)
            ((d) (proc d) #T))
          '())))))
  
  (define datum-find->list
    (case-lambda
      ((pred)
       (datum-find->list pred #F))
      ((pred want-warn)
       (lambda (start)
         (let ((r ((datum-find-enumerator pred want-warn) 
                   start
                   (if (path? start)
                     (lambda (d f a)
                       (values #T (assoc-update a f (lambda (x) (cons d x)) '())))
                     (lambda (d a)
                       (values #T (cons d a))))
                   '(()))))
           (if (path? start)
             (map (lambda (x) (cons (car x) (reverse (cdr x))))
                  (list-sort (lambda (x y) (string<? (car x) (car y))) r))
             (reverse r))))))) 

)
