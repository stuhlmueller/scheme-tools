#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

;; TODO?:  Get rid of value-directories?

(library (xitomatl file-system value-file)
  (export
    value-file? value-file-path make-value-file value-file=?
    value-file-ref value-file-set! delete-value-file
    value-directory? value-directory-path make-value-directory value-directory=?
    #;value-directory-ref value-directory-list value-directory-set! delete-value-directory)
  (import
    (except (rnrs) file-exists? delete-file)
    (only (xitomatl define) define/AV)
    (xitomatl file-system paths)
    (xitomatl file-system base))
  
  ;; value-file and value-directory objects are vectors instead of records so
  ;; they can be written and read using a portable external representation.
  
  (define (value-file? a)
    (and (vector? a)
         (= 2 (vector-length a))
         (eq? 'value-file (vector-ref a 0))
         (path? (vector-ref a 1))))
  
  (define (value-directory? a)
    (and (vector? a)
         (= 2 (vector-length a))
         (eq? 'value-directory (vector-ref a 0))
         (path? (vector-ref a 1))))
  
  (define (value-file-path vf)
    (vector-ref vf 1))
  
  (define (value-directory-path vd)
    (vector-ref vd 1))
  
  (define (make-value-file path)
    (vector 'value-file path))
  
  (define (make-value-directory path)
    (vector 'value-directory path))
  
  (define (value-file=? vf0 vf1)
    (path=? (value-file-path vf0) (value-file-path vf1)))  
  
  (define (value-directory=? vd0 vd1)
    (path=? (value-directory-path vd0) (value-directory-path vd1)))  
  
  (define delete-value-file 
    (case-lambda
      ((vf)
       (delete-file (value-file-path vf)))
      ((vd vf)
       (delete-file (path-join (value-directory-path vd) (value-file-path vf))))))
  
  (define (delete-value-directory vd)
    (delete-any (value-directory-path vd)))
  
  (define value-file-ref 
    (case-lambda
      ((vf) 
       (call-with-input-file (value-file-path vf) read))
      ((vd vf) 
       (call-with-input-file 
         (path-join (value-directory-path vd) (value-file-path vf))
         read))))
  
  #;(define/AV (value-directory-ref vd)
    (let ((d (value-directory-path vd)))
      (map (lambda (p)
             (let ((fp (path-join d p)))
               (cond ((file-regular? fp #F)
                      (cons (make-value-file p) 
                            (call-with-input-file fp read)))
                     ((file-directory? fp #F)
                      (cons (make-value-directory p) 
                            (value-directory-ref fp)))
                     (else (AV "not a file or directory" fp)))))
           (directory-list d))))
  
  (define/AV (value-directory-list vd)
    (let ((d (value-directory-path vd)))
      (map (lambda (p)
             (let ((fp (path-join d p)))
               (cond ((file-regular? fp #F) (make-value-file p))
                     ((file-directory? fp #F) (make-value-directory p))
                     (else (AV "not a file or directory" fp)))))
           (directory-list d))))
  
  (define value-file-set! 
    (let ((%value-file-set! (lambda (fn v)
                              (when (file-exists? fn #F)
                                (delete-any fn))
                              (call-with-port (OFOP fn)
                                (lambda (fop) (write v fop))))))
      (case-lambda
        ((vf v) 
         (%value-file-set! (value-file-path vf) v))
        ((vd vf v)
         (%value-file-set! 
          (path-join (value-directory-path vd) (value-file-path vf))
          v)))))

  (define/AV (value-directory-set! vd l)
    ;; Any duplicate path-names in l will overwrite each other, 
    ;; the last one will be the one remaining.
    (define (%value-directory-set! d l)      
      (make-directory d)
      (for-each 
        (lambda (x)
          (let ((v (car x)))
            (cond 
              ((value-file? v)
               (value-file-set! (path-join d (value-file-path v)) (cdr x)))
              ((value-directory? v)
               (%value-directory-set! (path-join d (value-directory-path v))
                                      (cdr x))))))
        l))
    (define (check l)
      (and (list? l)
           (for-all 
            (lambda (x)
              (and (pair? x)
                   (let ((v (car x)))
                     (or (and (value-file? v)
                              #;(relative-path? (_value-file-path v)))
                         (and (value-directory? v)
                              #;(relative-path? (_value-directory-path v))
                              (check (cdr x)))))))
            l)))
    (unless (check l) 
      (AV "not a valid association list of value-file or value-directory" l))
    ;; The above check of l must happen first to ensure no modification of the
    ;; file-system happens if l is invalid.
    (let ((d (value-directory-path vd)))
      (when (file-exists? d #F)
        (delete-any d))
      (%value-directory-set! d l)))
    
  (define (OFOP fn)
    (open-file-output-port fn (file-options no-fail)
                           (buffer-mode block) (native-transcoder)))
  
)
