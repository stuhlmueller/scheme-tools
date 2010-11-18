#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl lexer)
  (export
    token token? make-token token-lexeme
    counted-token counted-token? make-counted-token
    counted-token-char counted-token-line counted-token-column
    &lexer make-lexer-condition lexer-condition?
    lexer-condition-object lexer-condition-index
    lexer-enumerator lexer lexer/counting
    combine-tokenizers/precedence combine-tokenizers/exclusive
    chunk-tokenizer SRE-tokenizer
    simple-lexer simple-lexer/who define-simple-lexer)
  (import
    (rename (rnrs) (assert rnrs:assert))
    (only (xitomatl define) define/who)
    (except (xitomatl irregex) sre->irregex irregex-search/chunked)
    (xitomatl irregex)
    (xitomatl irregex extras)
    (xitomatl irregex counting)
    (only (xitomatl ports) textual-input-port?)
    (for (only (xitomatl macro-utils) identifier-append) expand))

  (define-syntax assert
    (syntax-rules ()
      ((_ expr) (rnrs:assert expr))
      #;((_ expr) #F)))

  ;;----------------------------------------------------------------------------

  (define-record-type token
    (fields lexeme))

  (define-record-type counted-token (parent token)
    (fields char line column))

  (define-condition-type &lexer &serious
    make-lexer-condition lexer-condition?
    (object lexer-condition-object)
    (index lexer-condition-index))

  ;;----------------------------------------------------------------------------

  (define (lexer-enumerator tokenizer)
    (lambda (obj proc seeds)
      (let loop ((o obj) (i 0) (s seeds))
        (let-values (((token next-obj next-idx) (tokenizer o i)))
          (unless token
            (raise (make-lexer-condition o i)))
          (let-values (((continue . next-seeds) (apply proc token s)))
            (if (and continue next-obj)
              (loop next-obj next-idx next-seeds)
              (apply values next-seeds)))))))

  (define (lexer tokenizer)
    (let ((le (lexer-enumerator tokenizer)))
      (lambda (obj)
        (reverse (le obj
                  (lambda (token accum) (values #T (cons token accum)))
                  '(()))))))

  (define (lexer/counting who make-tokenizer)
    (define (make chunker make-chunk)
      (let* ((c-chunker (counted-chunking-make-chunker chunker))
             (lxr (lexer (make-tokenizer c-chunker))))
        (lambda (obj)
          (lxr (counted-chunking-make-initial-chunk chunker (make-chunk obj))))))
    (let ((str-lxr (make list-chunker list))
          (tip-lxr (make port-chunker port-chunking-make-initial-chunk)))
      (lambda (obj)
        (cond ((string? obj) (str-lxr obj))
              ((textual-input-port? obj) (tip-lxr obj))
              (else (assertion-violation who "invalid object" obj))))))

  ;;----------------------------------------------------------------------------

  (define (combine-tokenizers/precedence . procs)
    (lambda (obj index)
      (let loop ((procs procs))
        (if (null? procs)
          (values #F #F #F)
          (let-values (((t no noi) ((car procs) obj index)))
            (if t
              (values t no noi)
              (loop (cdr procs))))))))

  (define (combine-tokenizers/exclusive . procs)
    (lambda (obj index)
      (let loop ((procs procs) (vals '(#F #F #F)))
        (if (null? procs)
          (apply values vals)
          (let-values (((t no noi) ((car procs) obj index)))
            (if t
              (if (car vals)
                (values #F #F #F)
                (loop (cdr procs) (list t no noi)))
              (loop (cdr procs) vals)))))))

  (define (chunk-tokenizer chunker tokenizer)
    (let ((get-start (chunker-get-start chunker))
          (get-end (chunker-get-end chunker))
          (get-next (chunker-get-next chunker)))
      (lambda (chunk index)
        (let ((i (+ (get-start chunk) index)))
          (let-values (((t nc nci) (tokenizer chunk i)))
            (if t
              (if (or (< nci (get-end nc))
                      (get-next nc))
                (values t nc (- nci (get-start nc)))
                (values t #F #F))
              (values #F #F #F)))))))

  (define (SRE-tokenizer sre)
    (let ((irx (sre->irregex `(: bos ,sre) 'fast)))
      (lambda (chunker make-token)
        (chunk-tokenizer chunker
         (lambda (chunk index)
           (let ((m (irregex-search/chunked irx chunker chunk index)))
             (if m
               (begin
                 (assert (and (eq? (irregex-match-start-chunk m 0) chunk)
                              (= (irregex-match-start-index m 0) index)))
                 (values (make-token m)
                         (irregex-match-end-chunk m 0)
                         (irregex-match-end-index m 0)))
               (values #F #F #F))))))))

  ;;----------------------------------------------------------------------------

  (define/who (simple-lexer combine-tokrs)
    (simple-lexer/who who combine-tokrs))

  (define (simple-lexer/who who combine-tokrs)
    (lambda specs
      (lexer/counting who
       (lambda (c-chunker)
         (apply combine-tokrs
                (map (lambda (x)
                       (if (pair? x)
                         ((car x) c-chunker (cdr x))
                         (x c-chunker)))
                     specs))))))

  (define (match->record make-record)
    (lambda (m)
      (let-values (((char line column) (counted-match-start-positions m)))
        (make-record (irregex-match-substring m) char line column))))

  (define-syntax define-simple-lexer
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name parent-type) (type spec) ...)
         (for-all identifier? #'(name parent-type type ...))
         (with-syntax (((spec-expr ...)
                        (map (lambda (x)
                               (syntax-case x (unquote)
                                 ((type (unquote make-tokr)) #'make-tokr)
                                 ((type sre)
                                  (with-syntax ((make-record (identifier-append #'type
                                                              "make-" #'type)))
                                    #'(cons (SRE-tokenizer sre)
                                            (match->record make-record))))))
                             #'((type spec) ...))))
           #'(begin
               (define-record-type type (parent parent-type)) ...
               (define name
                 ((simple-lexer/who 'name combine-tokenizers/precedence)
                  spec-expr ...)))))
        ((kw name (type spec) ...)
         (for-all identifier? #'(name type ...))
         #'(kw (name counted-token) (type spec) ...)))))
)
