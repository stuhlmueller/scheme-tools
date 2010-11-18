#!r6rs
;; Copyright 2010 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl irregex counting)
  (export
    line-separators chunk-counts chunk-counts/counted

    counted-chunk counted-chunk? make-counted-chunk
    counted-chunk-underlying counted-chunk-char counted-chunk-line
    counted-chunk-column counted-chunk-offset counted-chunk-line-seps
    counted-chunk-end-char counted-chunk-end-line counted-chunk-end-column
    counted-chunk-end-offset counted-chunk-next

    make-counted-chunk/count counted-chunking-make-initial-chunk
    counted-chunking-make-chunker counted-chunking-make-lose-refs
    prep-counted-chunking

    find-chunk/char find-chunk/char/counted
    line-column line-column/chunked line-column/chunked/counted

    find-chunk/line-column find-chunk/line-column/counted
    char-of-line-column char-of-line-column/chunked char-of-line-column/chunked/counted

    irregex-search/counting irregex-search/chunked/counting
    irregex-match/counting irregex-match/chunked/counting
    counted-match-start-positions counted-match-end-positions)
  (import
    (rename (rnrs) (assert rnrs:assert))
    (srfi :39 parameters)
    (only (xitomatl define) define/? define/AV)
    (only (xitomatl predicates) non-negative-integer?)
    (xitomatl irregex (or (0 7 (>= 4))
                          (0 (>= 8))
                          ((>= 1))))
    (only (xitomatl irregex extras) range-list-chunker make-lose-refs))

  (define-syntax assert
    (syntax-rules ()
      ((_ expr) (rnrs:assert expr))
      #;((_ expr) #F)))

  (define newline-rx)

  (define/AV line-separators
    ;; Default is Unicode line terminators.
    ;; From http://en.wikipedia.org/wiki/Newline#Unicode
    (make-parameter
     '("\xD;\xA;"  ;; CR+LF: CR followed by LF, U+000D followed by U+000A
       "\xD;"      ;; CR:    Carriage Return, U+000D
       "\xA;"      ;; LF:    Line Feed, U+000A
       "\x85;"     ;; NEL:   Next Line, U+0085
       "\xC;"      ;; FF:    Form Feed, U+000C
       "\x2028;"   ;; LS:    Line Separator, U+2028
       "\x2029;")  ;; PS:    Paragraph Separator, U+2029
     (lambda (x)
       (if (and (list? x)
                (for-all (lambda (y)
                           (and (string? y)
                                (or (= 1 (string-length y))
                                    (= 2 (string-length y)))))
                         x))
         (begin (set! newline-rx (sre->irregex `(or . ,x) 'fast))
                x)
         (AV "not a list of strings of length 1 or 2" x)))))

  (define (chunk-counts chunker chunk pos char line column offset)
    ;; NOTE: This assumes newline separators cannot span more than 2 chunks.
    ;; Because the chunks returned by any get-next must not represent an empty
    ;; string, and because newline-rx must match only 1 or 2 characters, this
    ;; assumption is okay.
    ;; NOTE: Some Unicode characters are displayed 2 columns wide, but this
    ;; procedure counts each character as 1 column regardless.
    (define (make-two-chunk str start end next)
      (cons* str start end (or next '())))
    (define two-chunk-start cadr)
    (define two-chunker range-list-chunker)
    (define get-str (chunker-get-str chunker))
    (define get-start (chunker-get-start chunker))
    (define get-end (chunker-get-end chunker))
    (define get-next (chunker-get-next chunker))
    (let ((str (get-str chunk))
          (start (get-start chunk))
          (end (get-end chunk))
          (next (get-next chunk)))
      (assert (<= start pos end))
      (assert (<= 0 offset (- end start)))
      (assert (or (= 0 offset) (= 0 column)))
      (let (#;(ne ???(and next (get-end next)))
            (ntc (and next (let ((str (get-str next))
                                 (start (get-start next))
                                 (end (get-end next)))
                             ;; The purpose of transforming to two-chunks is so
                             ;; the chain is maximum two long, so that
                             ;; irregex-search/chunked doesn't call get-next an
                             ;; unbounded number of times, which could be bad
                             ;; because it could unnecessarily consume memory
                             ;; and/or processor.  If in the future
                             ;; irregex-search/chunked supports an end bound,
                             ;; the commented-out code would be better.
                             (make-two-chunk str start end #F))))
            (lpos (- pos start)))
        (let ((two-chunk (make-two-chunk str start end ntc))
              (char (+ char lpos))
              (i (+ start offset)))
          (let loop ((i i)
                     (l line)
                     (seps (if (= 0 offset) '() (list (cons #F i)))))
            (let ((m (irregex-search/chunked
                      newline-rx two-chunker two-chunk i #;???next #;???ne)))
              (let ((msc (and m (irregex-match-start-chunk m 0)))
                    (msi (and m (irregex-match-start-index m 0))))
                (if (and (eq? msc two-chunk) (< msi pos))
                  (let ((mec (irregex-match-end-chunk m 0))
                        (mei (irregex-match-end-index m 0)))
                    (if (eq? mec two-chunk)
                      (let ((seps (cons (cons msi mei) seps)))
                        (if (< mei pos)
                          (loop mei (+ 1 l) seps)
                          (values char (+ 1 l) 0 (- mei pos) (reverse seps))))
                      (let ((mecs (two-chunk-start mec)))
                        (assert (eq? mec ntc))
                        (values char (+ 1 l) 0 (+ (- end pos) (- mei mecs))
                                (reverse (cons (cons msi #F) seps))))))
                  (values char l
                          (if (> l line)
                            (- pos i)
                            (if (< lpos offset)
                              column
                              (+ column (- lpos offset))))
                          (if (< lpos offset) (- offset lpos) 0)
                          (reverse seps))))))))))

  (define (chunk-counts/counted c-chunker c-chunk pos)
    (chunk-counts c-chunker c-chunk pos
     (counted-chunk-char c-chunk) (counted-chunk-line c-chunk)
     (counted-chunk-column c-chunk) (counted-chunk-offset c-chunk)))

  ;;--------------------------------------------------------------------------

  (define-record-type counted-chunk
    (fields underlying char line column offset
            line-seps end-char end-line end-column end-offset
            (mutable next)))

  (define (make-counted-chunk/count u-chunker u-chunk char line column offset next)
    (define get-end (chunker-get-end u-chunker))
    (let-values (((e-char e-line e-column e-offset line-seps)
                  (chunk-counts u-chunker u-chunk (get-end u-chunk)
                                char line column offset)))
      (make-counted-chunk u-chunk char line column offset
                          line-seps e-char e-line e-column e-offset
                          next)))

  (define (counted-chunking-make-initial-chunk u-chunker u-chunk)
    (make-counted-chunk/count u-chunker u-chunk 0 0 0 0 #F))

  (define (counted-chunking-make-chunker u-chunker)
    (define u-get-next (chunker-get-next u-chunker))
    (define u-get-str (chunker-get-str u-chunker))
    (define u-get-start (chunker-get-start u-chunker))
    (define u-get-end (chunker-get-end u-chunker))
    (define u-get-substring (chunker-get-substring u-chunker))
    (define u-get-subchunk (chunker-get-subchunk u-chunker))
    (define (get-next chunk)
      (or (counted-chunk-next chunk)
          (let* ((u (counted-chunk-underlying chunk))
                 (n (u-get-next u)))
            (and n
                 (let ((char (counted-chunk-end-char chunk))
                       (line (counted-chunk-end-line chunk))
                       (column (counted-chunk-end-column chunk))
                       (offset (counted-chunk-end-offset chunk)))
                   (let ((next (make-counted-chunk/count
                                u-chunker n char line column offset #F)))
                     (counted-chunk-next-set! chunk next)
                     next))))))
    (define (get-str chunk)
      (u-get-str (counted-chunk-underlying chunk)))
    (define (get-start chunk)
      (u-get-start (counted-chunk-underlying chunk)))
    (define (get-end chunk)
      (u-get-end (counted-chunk-underlying chunk)))
    (define (get-substring chunkA start chunkB end)
      (u-get-substring (counted-chunk-underlying chunkA) start
                       (counted-chunk-underlying chunkB) end))
    (define get-subchunk
      (and u-get-subchunk
           (lambda (chunkA start chunkB end)
             (let-values (((char line column offset line-seps)
                           (chunk-counts/counted chunker chunkA start)))
               (let ((ua (counted-chunk-underlying chunkA))
                     (ub (counted-chunk-underlying chunkB)))
                 (make-counted-chunk/count u-chunker
                  (u-get-subchunk ua start ub end)
                  char line column offset #F))))))
    (define chunker
      (make-irregex-chunker
       get-next get-str get-start get-end get-substring get-subchunk))
    chunker)

  (define helper-chunker
    (let ((A (lambda _ (rnrs:assert #F))))
      (make-irregex-chunker counted-chunk-next A A A A A)))

  (define (counted-chunking-make-lose-refs u-lose-refs)
    (let-values (((u-chunker u-make-chunk) (u-lose-refs)))
      (make-lose-refs helper-chunker
       (lambda (old new-next)
         (make-counted-chunk
          (u-make-chunk (counted-chunk-underlying old)
                        (and new-next (counted-chunk-underlying new-next)))
          (counted-chunk-char old) (counted-chunk-line old)
          (counted-chunk-column old) (counted-chunk-offset old)
          (counted-chunk-line-seps old)
          (counted-chunk-end-char old) (counted-chunk-end-line old)
          (counted-chunk-end-column old) (counted-chunk-end-offset old)
          new-next)))))

  (define prep-counted-chunking
    (case-lambda
      ((chunker chunk)
       (values (counted-chunking-make-chunker chunker)
               (counted-chunking-make-initial-chunk chunker chunk)))
      ((chunker chunk lose-refs)
       (values (counted-chunking-make-chunker chunker)
               (counted-chunking-make-initial-chunk chunker chunk)
               (counted-chunking-make-lose-refs lose-refs)))))

  ;;--------------------------------------------------------------------------

  (define/? (find-chunk/char chunker chunk (char non-negative-integer?))
    (define get-start (chunker-get-start chunker))
    (define get-end (chunker-get-end chunker))
    (define get-next (chunker-get-next chunker))
    (let loop ((i 0) (c chunk))
      (let* ((cs (get-start c))
             (ei (+ i (- (get-end c) cs))))
        (if (< char ei)
          (values c (+ cs (- char i)))
          (let ((n (get-next c)))
            (if n
              (loop ei n)
              (values #F #F)))))))

  (define/? (find-chunk/char/counted
             c-chunker c-chunk (char non-negative-integer?))
    (define get-next (chunker-get-next c-chunker))
    (define get-start (chunker-get-start c-chunker))
    (let loop ((cc c-chunk))
      (if (< char (counted-chunk-end-char cc))
        (let ((c (counted-chunk-char cc)))
          (if (< char c)
            (values #F #F)
            (values cc (+ (get-start cc) (- char c)))))
        (let ((n (get-next cc)))
          (if n
            (loop n)
            (values #F #F))))))

  (define (line-column str char)
    (line-column/chunked
     range-list-chunker (list str 0 (string-length str)) char))

  (define (line-column/chunked chunker chunk char)
    (let-values (((c-chunker c-chunk) (prep-counted-chunking chunker chunk)))
      (line-column/chunked/counted c-chunker c-chunk char)))

  (define (line-column/chunked/counted c-chunker c-chunk char)
    (let-values (((cc c) (find-chunk/char/counted c-chunker c-chunk char)))
      (if cc
        (let-values (((char line column offset line-seps)
                      (chunk-counts/counted c-chunker cc c)))
          (if (positive? offset)
            (values (- line 1) #F)
            (values line column)))
        (values #F #F))))

  (define (find-chunk/line-column chunker chunk line column)
    (let*-values (((c-chunker c-chunk)
                   (prep-counted-chunking chunker chunk))
                  ((cc i)
                   (find-chunk/line-column/counted c-chunker c-chunk line column)))
      (if cc
        (values (counted-chunk-underlying cc) i)
        (values #F #F))))

  (define/? (find-chunk/line-column/counted c-chunker c-chunk
             (line non-negative-integer?) (column non-negative-integer?))
    (define get-next (chunker-get-next c-chunker))
    (define get-start (chunker-get-start c-chunker))
    (let loop ((cc c-chunk))
      (let ((el (counted-chunk-end-line cc))
            (ecol (counted-chunk-end-column cc)))
        (if (or (< line el)
                (and (= line el) (< column ecol)))
          (let ((l (counted-chunk-line cc))
                (col (counted-chunk-column cc)))
            (if (or (< line l)
                    (and (= line l) (< column col)))
              (values #F #F)
              (let ((o (counted-chunk-offset cc))
                    (seps (counted-chunk-line-seps cc)))
                (let loop ((i (+ (get-start cc) o))
                           (l l)
                           (col col)
                           (seps (if (> o 0) (cdr seps) seps)))
                  (if (= l line)
                    (let ((c (+ i (- column col))))
                      (if (or (null? seps) (<= c (caar seps)))
                        (values cc c)
                        (values #F #F)))
                    (loop (cdar seps) (+ 1 l) 0 (cdr seps)))))))
          (let ((n (get-next cc)))
            (if n
              (loop n)
              (values #F #F)))))))

  (define (char-of-line-column str line column)
    (char-of-line-column/chunked
     range-list-chunker (list str 0 (string-length str)) line column))

  (define (char-of-line-column/chunked chunker chunk line column)
    (let-values (((c-chunker c-chunk) (prep-counted-chunking chunker chunk)))
      (char-of-line-column/chunked/counted c-chunker c-chunk line column)))

  (define (char-of-line-column/chunked/counted c-chunker c-chunk line column)
    (define get-start (chunker-get-start c-chunker))
    (let-values (((cc c)
                  (find-chunk/line-column/counted c-chunker c-chunk line column)))
      (and cc
           (+ (counted-chunk-char cc) (- c (get-start cc))))))

  ;;--------------------------------------------------------------------------

  (define (make-irregex-locator/chunked/counting locator)
    (lambda (irx chunker chunk . o)
      (let-values (((c-chunker c-chunk) (prep-counted-chunking chunker chunk)))
        (apply locator irx c-chunker c-chunk o))))

  (define (make-irregex-locator/counting locator/chunked/counting)
    (define locator/counting
      (case-lambda
        ((irx str)
         (locator/counting irx str 0))
        ((irx str start)
         (locator/counting irx str start (string-length str)))
        ((irx str start end)
         (locator/chunked/counting irx range-list-chunker (list str start end)))))
    locator/counting)

  (define irregex-search/chunked/counting
    (make-irregex-locator/chunked/counting irregex-search/chunked))

  (define irregex-search/counting
    (make-irregex-locator/counting irregex-search/chunked/counting))

  (define irregex-match/chunked/counting
    (make-irregex-locator/chunked/counting irregex-match/chunked))

  (define irregex-match/counting
    (make-irregex-locator/counting irregex-match/chunked/counting))

  ;;--------------------------------------------------------------------------

  (define (make-counted-match-positions source index)
    (define counted-match-positions
      (case-lambda
        ((m)
         (counted-match-positions m 0))
        ((m n)
         (let ((c (source m n)))
           (let-values (((char line column offset line-seps)
                         (chunk-counts/counted
                          (irregex-match-chunker m) c (index m n))))
             (if (positive? offset)
               (values char (- line 1) #F)
               (values char line column)))))))
    counted-match-positions)

  (define counted-match-start-positions
    (make-counted-match-positions irregex-match-start-chunk
                                  irregex-match-start-index))

  (define counted-match-end-positions
    (make-counted-match-positions irregex-match-end-chunk
                                  irregex-match-end-index))

)
