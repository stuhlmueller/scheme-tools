#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl irregex-tool)
  (export
    lines-enumerator
    single-enumerator)
  (import
    (except (rnrs) file-exists? delete-file)
    (xitomatl irregex)
    (xitomatl irregex extras)
    (xitomatl irregex counting)
    (xitomatl enumerators)
    (xitomatl file-system base)
    (xitomatl file-system paths))

  (define (files/dirs-enumerator files/dirs proc seeds)
    (define (apply-proc filename seeds)
      (call-with-input-file filename
        (lambda (fip)
          (let-values (((c . s) (apply proc fip filename seeds)))
            (values c s)))))
    (if (null? files/dirs)
      (let-values (((continue . seeds)
                    (apply proc (current-input-port) 'current-input-port seeds)))
        (apply values seeds))
      (let loop ((files/dirs files/dirs) (seeds seeds))
        (if (null? files/dirs)
          (apply values seeds)
          (let ((e (car files/dirs)))
            (let-values
                (((continue seeds)
                  (if (file-directory? e)
                    (fold/enumerator (directory-walk-enumerator) e
                     (lambda (path dirs files syms _ seeds)
                       (let-values
                           (((c s)
                             (fold/enumerator list-enumerator
                              (append files
                                      (filter (lambda (s)
                                                (file-regular? (path-join path s)))
                                              syms))
                              (lambda (f _ seeds)
                                (let-values (((c s)
                                              (apply-proc (path-join path f) seeds)))
                                  (values c c s)))
                              #T seeds)))
                         (let ((d (and c dirs)))
                           (values d d s))))
                     #T seeds)
                    (apply-proc e seeds))))
              (if continue
                (loop (cdr files/dirs) seeds)
                (apply values seeds))))))))

  (define (lines-enumerator irx)
    (let ((irx (irregex irx 'fast)))
      (lambda (files/dirs proc seeds)
        (let ((last-seeds
               (fold/enumerator files/dirs-enumerator files/dirs
                (lambda (fip filename seeds)
                  (let loop ((line-num 0) (seeds seeds))
                    (let ((l (get-line fip)))
                      (if (eof-object? l)
                        (values #T seeds)
                        (let ((m (irregex-search irx l)))
                          (if m
                            (let-values (((c . s)
                                          (apply proc filename line-num l m seeds)))
                              (if c
                                (loop (+ 1 line-num) s)
                                (values #F s)))
                            (loop (+ 1 line-num) seeds)))))))
                seeds)))
          (apply values last-seeds)))))

  (define (single-enumerator irx chunk-size)
    (define pc (make-port-chunker chunk-size))
    (define pe
      (irregex-chunk-enumerator
       (irregex irx 'single-line 'fast)
       (counted-chunking-make-chunker pc)
       (counted-chunking-make-lose-refs port-chunking-lose-refs)))
    (lambda (files/dirs proc seeds)
      (let ((last-seeds
             (fold/enumerator files/dirs-enumerator files/dirs
              (lambda (fip filename seeds)
                (fold/enumerator pe (counted-chunking-make-initial-chunk pc 
                                     (port-chunking-make-initial-chunk fip))
                 (lambda (m _ seeds)
                   (let-values (((c . s) (apply proc filename m seeds)))
                     (values c c s)))
                 #T seeds))
              seeds)))
        (apply values last-seeds))))
)
