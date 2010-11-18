#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl bytevectors)
  (export
    bytevector
    bytevector-concatenate
    bytevector-append
    subbytevector)
  (import
    (rnrs))
  
  (define (bytevector . u8s)
    (u8-list->bytevector u8s))
  
  (define (bytevector-concatenate bvs)
    (let ((n (make-bytevector
              (fold-left (lambda (len bv) (+ len (bytevector-length bv)))
                         0 bvs))))
      (let loop ((bvs bvs) (npos 0))
        (cond 
          ((null? bvs) n)
          (else 
           (let* ((bv (car bvs))
                  (len (bytevector-length bv)))
             (bytevector-copy! bv 0 n npos len)
             (loop (cdr bvs) (+ npos len))))))))
  
  (define (bytevector-append . bvs)
    (bytevector-concatenate bvs))

  (define (subbytevector bv start end) 
    (let* ((len (- end start))
           (n (make-bytevector len)))
      (bytevector-copy! bv start n 0 len)
      n))
  
)
