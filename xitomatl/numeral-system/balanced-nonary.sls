#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl numeral-system balanced-nonary)
  (export
    balanced-nonary-digits
    balanced-nonary->integer integer->balanced-nonary)
  (import
    (rnrs)
    (only (srfi :39 parameters) make-parameter)
    (only (xitomatl define) define/AV define/?)
    (only (xitomatl lists) unique?))

  (define/AV balanced-nonary-digits
    (make-parameter
     '((#\D . -4) (#\C . -3) (#\B . -2) (#\A . -1)
       (#\0 . 0) (#\1 . 1) (#\2 . 2) (#\3 . 3) (#\4 . 4))
     (lambda (x)
       (if (and (list? x)
                (= 9 (length x))
                (for-all pair? x)
                (let ((chars (map car x))
                      (ints (map cdr x)))
                  (and (for-all char? chars)
                       ((unique? char=?) chars)
                       #;(for-all exact-integer? ints)
                       #;((unique? =) ints)
                       (for-all (lambda (y) (memv y ints))
                                '(-4 -3 -2 -1 0 1 2 3 4)))))
         x
         (AV "invalid value" x)))))

  (define/? (balanced-nonary->integer (str string?))
    (define digits-alist (balanced-nonary-digits))
    (and (positive? (string-length str))
         (let loop ((l (reverse (string->list str))) (p 0) (n 0))
           (if (null? l) n
             (let ((d (assv (car l) digits-alist)))
               (and d (let ((dv (cdr d)) (pv (expt 9 p)))
                        (loop (cdr l) (+ 1 p) (+ n (* dv pv))))))))))

  (define/? (integer->balanced-nonary (int integer?))
    (define digits-alist (balanced-nonary-digits))
    (define (digit-of x)
      (let loop ((al digits-alist))
        (let ((p (car al)))
          (if (= x (cdr p))
            (car p)
            (loop (cdr al))))))
    (let recur ((i int) (a '()))
      (let-values (((d m) (div0-and-mod0 i 9)))
        (let ((a (cons (digit-of m) a)))
          (if (zero? d)
            (list->string a)
            (recur d a))))))
)
