#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl strings compat)
  (export
    string-copy!)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (xitomatl define) define/AV))

  (define/AV (string-copy! src src-start dst dst-start k)
    ;; Taken from Ikarus's implementation of string-copy!
    (cond
      ((or (not (fixnum? src-start)) (fx<? src-start 0))
       (AV "not a valid starting index" src-start))
      ((or (not (fixnum? dst-start)) (fx<? dst-start 0))
       (AV "not a valid starting index" dst-start))
      ((or (not (fixnum? k)) (fx<? k 0))
       (AV "not a valid length" k))
      ((not (string? src)) 
       (AV "not a string" src))
      ((not (string? dst)) 
       (AV "not a string" dst))
      ((fx>? (fx+ src-start k) (string-length src))
       (AV "out of range" src-start k))
      ((fx>? (fx+ dst-start k) (string-length dst))
       (AV "out of range" dst-start k))
      ((eq? src dst)
       (cond
         ((fx<? dst-start src-start)
          (let f ((src src) (si src-start) (di dst-start) (sj (fx+ src-start k)))
            (unless (fx=? si sj)
              (string-set! src di (string-ref src si))
              (f src (fx+ 1 si) (fx+ 1 di) sj))))
         ((fx<? src-start dst-start)
          (let f ((src src) (si (fx+ src-start k)) (di (fx+ dst-start k)) (sj src-start))
            (unless (fx=? si sj)
              (let ((si (fx- si 1)) (di (fx- di 1)))
                (string-set! src di (string-ref src si))
                (f src si di sj)))))
         (else (values))))
      (else
       (let f ((src src) (si src-start) (dst dst) (di dst-start) (sj (fx+ src-start k)))
         (unless (fx=? si sj)
           (string-set! dst di (string-ref src si))
           (f src (fx+ 1 si) dst (fx+ 1 di) sj))))))

)
