#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl lists)
  (export
    make-list last-pair ;; from compat
    sublist
    map/left-right/preserving map/filter
    remp-dups remove-dups remv-dups remq-dups
    dup? unique?
    intersperse group-by
    alist->plist plist->alist)
  (import
    (rnrs)
    (only (xitomatl define) define/? define/AV define/?/AV)
    (only (xitomatl predicates) exact-non-negative-integer? exact-positive-integer?)
    (xitomatl lists compat))

  (define/?/AV sublist
    (case-lambda/?
      ((l start)
       (sublist l start #F))
      ((l (start exact-non-negative-integer?)
          (end (lambda (x) (or (exact-non-negative-integer? x) (not x)))))
       (unless (or (not end) (<= start end))
         (AV "invalid range" start end)) 
       (let loop ((x l) (i 0) (a '()))
         (cond ((and end (= i end))
                (reverse a))
               ((pair? x)
                (loop (cdr x) (+ 1 i) (if (>= i start) (cons (car x) a) a)))
               ((null? x)
                (cond ((> start i)
                       (AV "start index greater than list's length" start i))
                      (end
                       (AV "end index greater than list's length" end i))
                      (else
                       (reverse a))))
               (else
                (AV "not a proper list" l)))))))
  
  ; Deterministic, left-to-right map
  ; It preserves sharing as much as possible: that is, if given the pair
  ; (h . t), (and (eq? h (f h)) 
  ;;              (eq? t (map/left-right/preserving f t))) 
  ;; holds, then
  ; (eq? (map/left-right/preserving f l) l) holds as well.
  (define/?/AV (map/left-right/preserving (f procedure?) l)
    ;; TODO: could this be made tail-recursive / constant space?
    (let loop ((f f) (l l) (orig l))
      (cond ((pair? l) (let ((h (car l)) (t (cdr l)))
                         (let* ((h1 (f h)) (t1 (loop f t orig)))
                           (if (and (eq? h1 h) (eq? t1 t)) 
                             l
                             (cons h1 t1))))) 
            ((null? l) '())
            (else (AV "not a proper list" orig)))))
  
  (define/?/AV map/filter 
    ;; map/filter is significantly more effecient than
    ;; the equivalent (filter values (map f l))
    (case-lambda/?
      (((f procedure?) l)
       (let loop ((l l) (r '()) (orig l))
         (cond ((pair? l) (let ((x (f (car l))))
                            (loop (cdr l) (if x (cons x r) r) orig)))
               ((null? l) (reverse r))
               (else (AV "not a proper list" orig)))))
      (((f procedure?) l . ls)
       (let loop ((ls (cons l ls)) (r '()) (orig (cons l ls)))
         (cond ((for-all pair? ls) (let ((x (apply f (map car ls))))
                                     (loop (map cdr ls) (if x (cons x r) r) orig)))
               ((for-all null? ls) (reverse r))
               (else (for-each (lambda (l o) (unless (or (pair? l) (null? l))
                                               (AV "not a proper list" o))) 
                               ls orig)
                     (for-each (lambda (l) (when (null? l)
                                             (AV "length mismatch" orig)))
                               ls)))))))

  (define (rem-dups rf l who)
    (let loop ((l l) (r '()))
      (cond ((pair? l) (let ((h (car l)) (t (cdr l)))
                         (loop (rf h t) (cons h r))))
            ((null? l) (reverse r))
            (else (assertion-violation who "not a proper list" l)))))
  (define (remp-dups proc l)
    (rem-dups proc l 'remp-dups))
  (define (remove-dups l)
    (rem-dups remove l 'remove-dups))
  (define (remv-dups l)
    (rem-dups remv l 'remv-dups))
  (define (remq-dups l)
    (rem-dups remq l 'remq-dups))

  (define/AV (dup? pred)
    (lambda (ls)
      (let loop ((l ls) (i 0))
        (cond ((pair? l) (let ((h (car l)) (t (cdr l)))
                           (if (memp (lambda (x) (pred h x)) t)
                             i
                             (loop t (+ 1 i)))))
              ((null? l) #F)
              (else (AV "not a proper list" ls))))))

  (define (unique? pred)
    (define f (dup? pred))
    (lambda (l) (not (f l))))
  
  (define/AV (intersperse l sep)
    (let loop ((l l) (r '()) (sep sep) (orig l))
      (cond ((pair? l) (loop (cdr l) (cons* sep (car l) r) sep orig))
            ((null? l) (if (null? r) '() (reverse (cdr r))))
            (else (AV "not a proper list" orig)))))

  (define/?/AV group-by
    (case-lambda/?
      ((by)
       (group-by by #F))
      (((by exact-positive-integer?) remainders-ok?)
       (lambda (l)
         (let loop ((l l) (n 0) (g '()) (r '()))
           (cond ((pair? l)
                  (if (= n by)
                    (loop (cdr l) 1 (list (car l)) (cons (reverse g) r))
                    (loop (cdr l) (+ 1 n) (cons (car l) g) r)))
                 ((null? l)
                  (cond ((or (= n by) (and remainders-ok? (> n 0)))
                         (reverse (cons (reverse g) r)))
                        ((= n 0) '())
                        (else (apply AV "remainder elements" (reverse g)))))
                 (else (AV "not a proper list"))))))))

  (define/AV (alist->plist x)
    (let loop ((al x) (pl '()))
      (cond ((and (pair? al) (pair? (car al)))
             (loop (cdr al) (cons* (cdar al) (caar al) pl)))
            ((null? al) (reverse pl))
            (else (AV "not a proper alist" x)))))

  (define/AV (plist->alist x)
    (let loop ((pl x) (al '()))
      (cond ((and (pair? pl) (pair? (cdr pl)))
             (loop (cddr pl) (cons (cons (car pl) (cadr pl)) al)))
            ((null? pl) (reverse al))
            (else (AV "not a proper plist" x)))))
  
  #;(define/? (flatten (l list?))
    ;; not sure exactly what I want the semantics to be
    (let loop ((l l))
      (if (pair? l)
        (let ((x (car l)) (r (cdr l)))
          (if (list? x)
            (append (loop x) (loop r))
            (cons (loop x) (loop r))))
        l)))

)
