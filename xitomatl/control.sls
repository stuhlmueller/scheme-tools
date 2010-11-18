#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl control)
  (export
    begin0
    compose)
  (import 
    (rnrs))
  
  (define-syntax begin0
    (syntax-rules ()
      ((_ form0 form1 ...)
       (let ((result form0))
         (begin form1 ... result)))))

  (define (compose . procs)
    (let ((procs (reverse procs)))
      (lambda args
        (let loop ((procs procs) (args args))
          (if (null? procs)
            (apply values args)
            (let-values ((vals (apply (car procs) args)))
              (loop (cdr procs) vals)))))))
)
