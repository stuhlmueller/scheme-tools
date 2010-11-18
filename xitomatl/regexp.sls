#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

;; FIXME?: Is 'bos 2nd ret-val idea flawed for future negative-look-behind?
;; N.L.B. might have a 'bos, and it would fail at first until having moved down
;; the string, but maybe the 'bos idea doesn't work with that?  But maybe the
;; N.L.B. matcher will compose correctly with the other matchers and it actually
;; will work.

;; TODO: Group capturing.

(library (xitomatl regexp)
  (export
    regexp-search
    regexp-search/chunked
    regexp-match
    regexp-match/chunked
    compile-SRE)
  (import
    (rename (rnrs)
            (for-all andmap)
            (exists ormap))
    (only (xitomatl define)
          define/AV)
    (only (xitomatl predicates)
          non-negative-integer?)
    (only (xitomatl match)
          match-lambda)
    (xitomatl irregex)
    (only (xitomatl irregex extras)
          range-list-chunker)
    (for (only (xitomatl macro-utils)
               identifier-append)
         expand))

  (define regexp-search/chunked
    (case-lambda
      ((sre chunker chunk)
       (regexp-search/chunked sre chunker chunk ((chunker-get-start chunker) chunk)))
      ((sre chunker chunk idx)
       (let ((m (compile-SRE sre)) (r (ref chunker)))
         (let-values (((c o i) (r chunk idx)))
           (let loop ((o o) (i i) (b i))
             (let-values (((o^ i^) (m o i b r values)))
               (if o^
                 (let ((im (make-irregex-match 0 '())))
                   ;; TODO: Supporting groups will require different arguments to
                   ;; above call to make-irregex-match.
                   (irregex-match-chunker-set! im chunker)
                   (irregex-match-start-chunk-set! im 0 o)
                   (irregex-match-start-index-set! im 0 i)
                   (irregex-match-end-chunk-set! im 0 o^)
                   (irregex-match-end-index-set! im 0 i^)
                   im)
                 (and (not (eq? i^ 'bos))
                      (let-values (((c o i) (r o i)))
                        (and c (loop o (+ 1 i) #F))))))))))))

  (define regexp-search
    (case-lambda
      ((sre str) (regexp-search sre str 0))
      ((sre str start) (regexp-search sre str start (string-length str)))
      ((sre str start end)
       (regexp-search/chunked sre range-list-chunker (list str start end) start))))

  (define (regexp-match/chunked sre chunker chunk . a)
    (apply regexp-search/chunked `(: bos ,sre eos) chunker chunk a))

  (define (regexp-match sre str . a)
    (apply regexp-search `(: bos ,sre eos) str a))

  ;;----------------------------------------------------------------------------

  (define/AV (ref chunker)
    (define get-start (chunker-get-start chunker))
    (define get-end (chunker-get-end chunker))
    (define get-str (chunker-get-str chunker))
    (define get-next (chunker-get-next chunker))
    (lambda (chunk idx)
      (let ((s (get-start chunk)) (e (get-end chunk)))
        (cond ((and (<= s idx) (< idx e))
               (values (string-ref (get-str chunk) idx) chunk idx))
              ((= idx e)
               (let ((n (get-next chunk)))
                 (if n
                   (let ((ns (get-start n)))
                     (values (string-ref (get-str n) ns) n ns))
                   (values #F chunk idx))))
              (else
               (AV "invalid index" idx s e))))))

  ;;----------------------------------------------------------------------------

  (define (M-char c)
    ;; TODO?: Use char-set of size 1 instead?
    (lambda (o i b r k)
      (let-values (((c2 o i) (r o i)))
        (if (and c2 (char=? c2 c))
          (k o (+ 1 i))
          (k #F #F)))))

  (define (M-str str)
    (let ((l (string-length str)))
      (lambda (o i b r k)
        (let loop ((o o) (i i) (x 0))
          (if (= x l)
            (k o i)
            (let-values (((c o i) (r o i)))
              (if (and c (char=? c (string-ref str x)))
                (loop o (+ 1 i) (+ 1 x))
                (k #F #F))))))))

  ;; TODO: Optimize char sets to consolidate as much as possible.

  (define (M-/ ranges)
    (lambda (o i b r k)
      (let-values (((c o i) (r o i)))
        (if (and c
                 (let loop ((ranges ranges))
                   (and (pair? ranges)
                        (or (char<=? (car ranges) c (cadr ranges))
                            (loop (cddr ranges))))))
          (k o (+ 1 i))
          (k #F #F)))))

  (define (M-~ m)
    (lambda (o i b r k)
      (let-values (((c o i) (r o i)))
        (if c
          (let loop ((m m))
            (if (null? m)
              (k o (+ 1 i))
              ;; NOTE: b can be ignored because all sub-matchers must be only
              ;; char-set matchers, and they never need b.
              ((car m) o i #F r
               (lambda (o^ i^)
                 (if o^
                   (k #F #F)
                   (loop (cdr m)))))))
          (k #F #F)))))

  (define (M-& m)
    (if (positive? (length m))
      (lambda (o i b r k)
        (let-values (((c o i) (r o i)))
          (if c
            (let loop ((m m))
              (if (null? m)
                (k o (+ 1 i))
                ;; NOTE: It's okay to ignore b, as described above.
                ((car m) o i #F r
                 (lambda (o^ i^)
                   (if o^
                     (loop (cdr m))
                     (k #F i^))))))
            (k #F #F))))
      M-false))

  ;; TODO: Optimize compound matchers for (= 1 (length m)).

  (define (M-: m)
    (lambda (o i b r k)
      (let loop ((m m) (o o) (i i) (b b))
        (if (null? m)
          (k o i)
          ((car m) o i b r
           (lambda (o^ i^)
             (if o^
               (loop (cdr m) o^ i^ (and (eq? o^ o) b))
               (k #F i^))))))))

  (define (M-or m cs?)
    ;; TODO: Optimize successive chars as a char-set.
    ;; TODO: Only chars is a proper char-set.
    (lambda (o i b r k)
      (let loop ((m m) (il '()))
        (define (fail-info)
          (and (pair? il)
               (andmap (lambda (x) (eq? 'bos x)) il)
               'bos))
        (if (null? m)
          (k #F (fail-info))
          ((car m) o i b r
           (lambda (o^ i^)
             (if o^
               (if cs?
                 (k o^ i^)
                 (let-values (((o^^ i^^) (k o^ i^)))
                   (if o^^
                     (values o^^ i^^)
                     (loop (cdr m) (cons i^^ il)))))
               (loop (cdr m) (cons i^ il)))))))))

  (define (M-** min max m)
    (lambda (o i b r k)
      (let loop ((o o) (i i) (b b) (n 0) (p '((#F . #F))))
        (define (k/try i^)
          (let back ((o o) (i i) (n n) (p p) (i^ i^))
            (if (>= n min)
              (let-values (((o^ i^) (k o i)))
                (if o^
                  (values o^ i^)
                  (back (caar p) (cdar p) (- n 1) (cdr p) i^)))
              (k #F i^))))
        (if (and (or (not max) (< n max))
                 (not (and (eq? o (caar p)) (= i (cdar p)))))
          (m o i b r
           (lambda (o^ i^)
             (if o^
               (loop o^ i^ (and (eq? o^ o) b) (+ 1 n) (cons (cons o i) p))
               (k/try i^))))
          (k/try #F)))))

  (define (M-**? min max m)
    (lambda (o i b r k)
      (let loop ((o o) (i i) (b b) (n 0) (po #F) (pi #F))
        (define (try)
          (if (and (or (not max) (< n max))
                   (not (and (eq? o po) (= i pi))))
            (m o i b r
             (lambda (o^ i^)
               (if o^
                 (loop o^ i^ (and (eq? o^ o) b) (+ 1 n) o i)
                 (k #F #F))))
            (k #F #F)))
        (if (>= n min)
          (let-values (((o^ i^) (k o i)))
            (if o^
              (values o^ i^)
              (try)))
          (try)))))

  (define (M-look-ahead m)
    (lambda (o i b r k)
      (m o i b r
       (lambda (o^ i^)
         (if o^
           (k o i)
           (k #F i^))))))

  (define (M-false o i b r k)
    (k #F #F))

  (define (M-char/pred pred)
    (lambda (o i b r k)
      (let-values (((c o i) (r o i)))
        (if (and c (pred c))
          (k o (+ 1 i))
          (k #F #F)))))

#;(define (M-proc proc)
    (lambda (o i b r k)
      (let-values (((o^ i^) (proc o i b r)))
        (k o^ i^))))

  ;;----------------------------------------------------------------------------

  (define (valid-range? min max)
    (and (non-negative-integer? min)
         (or (not max)
             (and (non-negative-integer? max)
                  (<= min max)))))

  (define (valid-char-ranges? x)
    (or (null? x)
        (and (pair? x) (pair? (cdr x))
             (char? (car x)) (char? (cadr x))
             (char<=? (car x) (cadr x))
             (valid-char-ranges? (cddr x)))))

  (define valid-char-set?
    (match-lambda
      ((:predicate char?) #T)
      (('/ c ...)
       (valid-char-ranges? c))
      (((:or 'or '~ '- '&) x ...)
       (andmap valid-char-set? x))
      (x (symbol? x)
       (memq x named-char-sets))
      (_ #F)))

  (define/AV compile-SRE
    ;; TODO?: Don't use match-lambda and instead do it more manually, for efficiency.
    (match-lambda
      (x (symbol? x)
       (or (named-M x)
           (AV "unknown named SRE" x)))
      (x (char? x)
       (M-char x))
      (x (string? x)
       (M-str x))
      (('/ c ...)
       (valid-char-ranges? c)
       (M-/ c))
      (('~ cs ...)
       (andmap valid-char-set? cs)
       (M-~ (map compile-SRE cs)))
      (('- cs ...)
       (andmap valid-char-set? cs)
       (if (pair? cs)
         (compile-SRE `(& ,(car cs) (~ . ,(cdr cs))))
         M-false))
      (('& cs ...)
       (andmap valid-char-set? cs)
       (M-& (map compile-SRE cs)))
      ((': x ...)
       (M-: (map compile-SRE x)))
      (('or x ...)
       (M-or (map compile-SRE x) (andmap valid-char-set? x)))
      (('* x ...)
       (compile-SRE `(** 0 #F . ,x)))
      (('*? x ...)
       (compile-SRE `(**? 0 #F . ,x)))
      (('+ x ...)
       (compile-SRE `(** 1 #F . ,x)))
    #|(('+? x ...)  ;; invalid symbol, what to do...?
       (compile-SRE `(**? 1 #F . ,x)))|#
      (('? x ...)
       (compile-SRE `(** 0 1 . ,x)))
      (('?? x ...)
       (compile-SRE `(**? 0 1 . ,x)))
      (('= n x ...)
       (non-negative-integer? n)
       (compile-SRE `(** ,n ,n . ,x)))
      (('>= n x ...)
       (non-negative-integer? n)
       (compile-SRE `(** ,n #F . ,x)))
      (('>=? n x ...)
       (non-negative-integer? n)
       (compile-SRE `(**? ,n #F . ,x)))
      (('** min max x ...)
       (valid-range? min max)
       (M-** min max (compile-SRE `(: . ,x))))
      (('**? min max x ...)
       (valid-range? min max)
       (M-**? min max (compile-SRE `(: . ,x))))
      (('look-ahead x ...)
       (M-look-ahead (compile-SRE `(: . ,x))))
      (x (procedure? x)
       x)
      (#F
       M-false)
      (x
       (AV "invalid SRE" x))))

  ;;----------------------------------------------------------------------------

  (define (named-M name)
    (cond ((assq name named-M-alist) => cdr)
          (else #F)))

  (define named-M-alist '())
  (define named-char-sets '())

  (define (named-M-add! name sre cs?)
    (define M (compile-SRE sre))
    (set! named-M-alist (cons (cons name M) named-M-alist))
    (when cs?
      (set! named-char-sets (cons name named-char-sets)))
    M)

  (define-syntax define-named-M
    (lambda (stx)
      (syntax-case stx ()
        ((kw ((SRE-name M-name) flag ... expr) . r)
         (andmap identifier? (list #'SRE-name #'M-name))
         (with-syntax ((cs? (and (memq ':cs (syntax->datum #'(flag ...))) #T)))
           #'(begin
               (define M-name (named-M-add! 'SRE-name expr cs?))
               (kw . r))))
        ((kw (SRE-name . cr) . r)
         (identifier? #'SRE-name)
         (with-syntax ((M-name (identifier-append #'SRE-name "M-" #'SRE-name)))
           #'(kw ((SRE-name M-name) . cr) . r)))
        ((_)
         #'(begin)))))

  (define-named-M
    ;; NOTE: These are pre-compiled, so they won't work for possible future
    ;; compile-SRE needing to recursively pass-through info.  So, they should
    ;; all be patterns which do not involve anything which would require that.
    (bos
     (lambda (o i b r k)
       (if (and b (= i b))
         (k o i)
         (k #F 'bos))))
    (eos
     (lambda (o i b r k)
       (let-values (((c o i) (r o i)))
         (if c
           (k #F #F)
           (k o i)))))
    (any :cs
     (lambda (o i b r k)
       (let-values (((c o i) (r o i)))
         (if c
           (k o (+ 1 i))
           (k #F #F)))))
    (newline-char :cs
     '(or #\xA #\xD #\x85 #\xC #\x2028 #\x2029))
    (nonl :cs
     '(- any newline-char))
    (alphabetic :cs
     (M-char/pred char-alphabetic?))
    (numeric :cs
     (M-char/pred char-numeric?))
    (whitespace :cs
     (M-char/pred char-whitespace?))
    (digit :cs
     '(/ #\0 #\9))
    (hex-digit :cs
     '(or digit (/ #\a #\f #\A #\F))))

  (define (M-gen-cat gc)
    (M-char/pred (lambda (c) (eq? gc (char-general-category c)))))

  (define-syntax define-gen-cat-M
    (syntax-rules ()
      ((_ name ...)
       (define-named-M
         (name :cs (M-gen-cat 'name))
         ...))))

  (define-gen-cat-M Lu Ll Lt Lm Lo Mn Mc Me Nd Nl No Pc Pd Ps Pe
                    Pi Pf Po Sm Sc Sk So Zs Zl Zp Cc Cf Cs Co Cn)

)
