#!r6rs
;; Copyright 2010 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl irregex extras)
  (export
    irregex-search/all irregex-search/all/strings
    irregex-search/chunked/all irregex-search/chunked/all/strings
    irregex-search-port/all irregex-search-port/all/strings
    irregex-chunk-enumerator irregex-string-enumerator
    irregex-list-enumerator irregex-port-enumerator irregex-enumerator
    list-chunker list-chunking-lose-refs
    range-list-chunker range-list-chunking-lose-refs
    port-chunker make-port-chunker port-chunking-lose-refs
    port-chunking-make-initial-chunk
    make-lose-refs chunk-eqv? chunk-equal?)
  (import
    (rename (rnrs) (assert rnrs:assert))
    (only (xitomatl define) define/? define/AV)
    (only (xitomatl predicates) exact-positive-integer? or?)
    (only (xitomatl ports) textual-input-port?)
    (only (xitomatl enumerators) fold/enumerator)
    (xitomatl irregex (or (0 7 (>= 3))
                          (0 (>= 8))
                          ((>= 1)))))

  (define-syntax assert
    (syntax-rules ()
      ((_ expr) (rnrs:assert expr))
      #;((_ expr) #F)))

  (define irregex-search/all
    (case-lambda
      ((irx str)
       (irregex-search/all irx str 0))
      ((irx str start)
       (irregex-search/all irx str start (string-length str)))
      ((irx str start end)
       (irregex-search/all irx str start end values))
      ((irx str start end proc)
       (reverse
        (fold/enumerator (irregex-string-enumerator irx start end)
                         str
                         (lambda (m a) (values #T (cons (proc m) a)))
                         '())))))

  (define irregex-search/all/strings
    (case-lambda
      ((irx str)
       (irregex-search/all/strings irx str 0))
      ((irx str start)
       (irregex-search/all/strings irx str start (string-length str)))
      ((irx str start end)
       (irregex-search/all irx str start end irregex-match-substring))))

  (define irregex-search/chunked/all
    (case-lambda
      ((irx chunker chunk)
       (irregex-search/chunked/all irx chunker chunk #F))
      ((irx chunker chunk lose-refs)
       (irregex-search/chunked/all irx chunker chunk lose-refs values))
      ((irx chunker chunk lose-refs proc)
       (reverse
        (fold/enumerator (irregex-chunk-enumerator irx chunker lose-refs)
                         chunk
                         (lambda (m a) (values #T (cons (proc m) a)))
                         '())))))

  (define (irregex-search/chunked/all/strings irx chunker chunk)
    ;; NOTE: Don't need to supply a lose-refs because the match objects
    ;;       are immediately lost after given to irregex-match-substring.
    (irregex-search/chunked/all irx chunker chunk #F irregex-match-substring))

  (define irregex-search-port/all
    (case-lambda
      ((irx port)
       (irregex-search-port/all irx port values))
      ((irx port proc)
       (irregex-search-port/all irx port proc #F))
      ((irx port proc chunk-size)
       (irregex-search/chunked/all irx (if chunk-size
                                         (make-port-chunker chunk-size)
                                         port-chunker)
                                   (port-chunking-make-initial-chunk port)
                                   port-chunking-lose-refs proc))))

  (define irregex-search-port/all/strings
    (case-lambda
      ((irx port)
       (irregex-search-port/all/strings irx port #F))
      ((irx port chunk-size)
       (irregex-search/chunked/all/strings irx (if chunk-size
                                                 (make-port-chunker chunk-size)
                                                 port-chunker)
                                           (port-chunking-make-initial-chunk port)))))

  ;;--------------------------------------------------------------------------

  (define/AV irregex-chunk-enumerator
    (case-lambda
      ((irx chunker)
       (irregex-chunk-enumerator irx chunker #F))
      ((irx chunker lose-refs)
       (let ((irx-c (irregex irx))
             (get-start (chunker-get-start chunker)))
         (lambda (chunk proc seeds)
           (let loop ((chk chunk) (i (get-start chunk)) (seeds seeds))
             (let ((m (irregex-search/chunked irx-c chunker chk i)))
               (if m
                 (let ((end-chunk (irregex-match-end-chunk m 0))
                       (end-index (irregex-match-end-index m 0)))
                   (when lose-refs
                     ;; Losing possible reference(s) reachable from the match
                     ;; object to chunk(s) outside the match chunks is done to
                     ;; allow outside chunk(s) to be GC'ed when they're no
                     ;; longer needed during the search, which is necessary for
                     ;; efficient memory usage.  lose-refs must return chunks
                     ;; which will work with chunker's procedures.  lose-refs
                     ;; must not mutate the chunks given to it, it must return
                     ;; new chunk objects referring to the same underlying
                     ;; string pieces but not referring to chunks outside the
                     ;; match range.  The only thing mutated is the new match
                     ;; object which was made just for us.
                     (let ((replacements
                            (let loop ((n (irregex-match-num-submatches m))
                                       (submatch-chunks '()))
                              (if (negative? n)
                                (lose-refs submatch-chunks)
                                (loop (- n 1)
                                      (if (irregex-match-valid-index? m n)
                                        (cons (list n
                                                    (irregex-match-start-chunk m n)
                                                    (irregex-match-end-chunk m n))
                                              submatch-chunks)
                                        submatch-chunks))))))
                       (for-each (lambda (x)
                                   (let ((n (car x)) (s (cadr x)) (e (caddr x)))
                                     (irregex-match-start-chunk-set! m n s)
                                     (irregex-match-end-chunk-set! m n e)))
                                 replacements)))
                   (let-values (((continue . next-seeds) (apply proc m seeds)))
                     (if continue
                       (if (or (not (eq? chk end-chunk))
                               (< i end-index))
                         (loop end-chunk end-index next-seeds)
                         (AV "pattern not advancing search" irx))
                       (apply values next-seeds))))
                 (apply values seeds)))))))))

  (define irregex-string-enumerator
    (case-lambda
      ((irx)
       (irregex-string-enumerator irx 0))
      ((irx start)
       (irregex-string-enumerator irx start #F))
      ((irx start end)
       (let ((ce (irregex-chunk-enumerator irx range-list-chunker)))
         (lambda (str proc seeds)
           (ce (list str start (or end (string-length str)))
               proc seeds))))))

  (define (irregex-list-enumerator irx)
    (let ((ce (irregex-chunk-enumerator irx list-chunker list-chunking-lose-refs)))
      (lambda (l proc seeds)
        (if (null? l)
          (apply values seeds)
          (ce l proc seeds)))))

  (define irregex-port-enumerator
    (case-lambda
      ((irx)
       (irregex-port-enumerator irx #F))
      ((irx chunk-size)
       (let ((ce (irregex-chunk-enumerator irx (if chunk-size
                                                 (make-port-chunker chunk-size)
                                                 port-chunker)
                                           port-chunking-lose-refs)))
         (lambda (port proc seeds)
           (ce (port-chunking-make-initial-chunk port) proc seeds))))))

  (define/AV (irregex-enumerator irx)
    (lambda (coll proc seeds)
      (cond
        ((string? coll)
         ((irregex-string-enumerator irx) coll proc seeds))
        ((list? coll)
         ((irregex-list-enumerator irx) coll proc seeds))
        ((textual-input-port? coll)
         ((irregex-port-enumerator irx) coll proc seeds))
        (else
         (AV "invalid collection type" coll)))))

  ;;--------------------------------------------------------------------------

  (define (make-lose-refs chunker make-chunk)
    (let ((get-next (chunker-get-next chunker)))
      (case-lambda
        (() (values chunker make-chunk))
        ((submatch-chunks)
         ;; submatch-chunks ::= ((0 <start-chunk> <end-chunk>)
         ;;                      (X <start-chunk> <end-chunk>) ...)
         (let ((first (cadar submatch-chunks))
               (last (caddar submatch-chunks)))
           (let* ((reversed-chain
                   (let loop ((chain first) (rev '()))
                     (if (eq? chain last)
                       (cons chain rev)
                       (loop (get-next chain) (cons chain rev)))))
                  (correlated
                   (let loop ((rev reversed-chain) (next #F) (alist '()))
                     (if (null? rev)
                       alist
                       (let* ((o (car rev))
                              (nc (make-chunk o next)))
                         (loop (cdr rev) nc (cons (cons o nc) alist)))))))
             (map (lambda (x)
                    (list (car x)
                          (cdr (assq (cadr x) correlated))
                          (cdr (assq (caddr x) correlated))))
                  submatch-chunks)))))))

  (define (chunk-eqv? chunker)
    (let ((get-str (chunker-get-str chunker))
          (get-start (chunker-get-start chunker))
          (get-end (chunker-get-end chunker)))
      (lambda (x y)
        (and (eq? (get-str x) (get-str y))
             (= (get-start x) (get-start y))
             (= (get-end x) (get-end y))))))

  (define (chunk-equal? chunker)
    (let ((get-str (chunker-get-str chunker))
          (get-start (chunker-get-start chunker))
          (get-end (chunker-get-end chunker)))
      (lambda (x y)
      #;(string=? (substring (get-str x) (get-start x) (get-end x))
                  (substring (get-str y) (get-start y) (get-end y)))
        (let ((xstr (get-str x)) (xs (get-start x)) (xe (get-end x))
              (ystr (get-str y)) (ys (get-start y)) (ye (get-end y)))
          (and (= (- xe xs) (- ye ys))
               (let loop ((xi xs) (yi ys))
                 (or (= xi xe)
                     (and (char=? (string-ref xstr xi) (string-ref ystr yi))
                          (loop (+ 1 xi) (+ 1 yi))))))))))

  ;;--------------------------------------------------------------------------

  (define list-chunker
    (make-irregex-chunker
     (letrec ((get-next (lambda (chunk)
                          (let ((r (cdr chunk)))
                            (and (pair? r)
                                 (if (string=? "" (car r))
                                   (get-next r)
                                   r))))))
       get-next)
     car))

  (define list-chunking-lose-refs
    (make-lose-refs list-chunker
                    (lambda (old-chunk new-next-chunk)
                      (cons (car old-chunk)
                            (or new-next-chunk '())))))

  (define range-list-chunker
    (make-irregex-chunker
     (letrec ((get-next (lambda (chunk)
                          (let ((r (cdddr chunk)))
                            (and (pair? r)
                                 (if (string=? "" (car r))
                                   (get-next r)
                                   r))))))
       get-next)
     car cadr caddr))

  (define range-list-chunking-lose-refs
    (make-lose-refs range-list-chunker
                    (lambda (old-chunk new-next-chunk)
                      (cons* (car old-chunk)
                             (cadr old-chunk)
                             (caddr old-chunk)
                             (or new-next-chunk '())))))

  (define-record-type port-chunk (fields str (mutable next)))

  (define/? (make-port-chunker (chunk-size exact-positive-integer?))
    (make-irregex-chunker
     (lambda (chunk)
       (let ((n (port-chunk-next chunk)))
         (if (port? n)
           (let* ((s (get-string-n n chunk-size))
                  (next (and (string? s) (make-port-chunk s n))))
             (port-chunk-next-set! chunk next)
             next)
           n)))
     port-chunk-str))

  (define port-chunker (make-port-chunker #x400))  ;; good default size?

  (define port-chunking-lose-refs
    (make-lose-refs port-chunker
                    (lambda (old-chunk new-next-chunk)
                      (make-port-chunk (port-chunk-str old-chunk)
                                       new-next-chunk))))

  (define (port-chunking-make-initial-chunk port)
    (make-port-chunk "" port))
)
