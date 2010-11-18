#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl R6RS-lexical-transformations)
  (export
    #;delete-trailing-whitespace
    brackets->parens
    upcase-booleans
    hashbang-r6rs-first
    transform-file
    transform-walk
    append-lexemes)
  (import
    (rnrs)
    (srfi :2 and-let*)
    (only (xitomatl define) define/who)
    (only (xitomatl ports) textual-input-port?)
    (only (xitomatl records) record-type-accessors)
    (only (xitomatl enumerators) fold)
    (only (xitomatl predicates) not?)
    (only (xitomatl file-system base) directory-walk)
    (only (xitomatl file-system paths) path-join)
    (only (xitomatl common) printf)
    (only (xitomatl lexer) token-lexeme)
    (xitomatl R6RS-lexer))

  (define (append-lexemes tokens)
    (apply string-append (map token-lexeme tokens)))

  (define (make-transformation who proc)
    (define (t x)
      (define (lex p)
        (and-let* ((l (t (R6RS-lexer x))))
          (p l)))
      (cond ((list? x)
             (proc x))
            ((textual-input-port? x)
             (lex (lambda (l) (open-string-input-port (append-lexemes l)))))
            ((string? x)
             (lex append-lexemes))
            (else
             (assertion-violation who "invalid argument" x))))
    t)

  (define (get-fields x)
    (map (lambda (a) (a x)) (record-type-accessors (record-rtd x))))

#;(define/who delete-trailing-whitespace
    TODO)

  (define/who brackets->parens
    (make-transformation who
     (lambda (tokens)
       (let-values (((accum modified?)
                     (fold tokens
                      (lambda (t a m)
                        (define (trans make lexeme)
                          (apply make lexeme (cdr (get-fields t))))
                        (cond ((open-bracket-token? t)
                               (values #T
                                       (cons (trans make-open-paren-token "(") a)
                                       #T))
                              ((close-bracket-token? t)
                               (values #T
                                       (cons (trans make-close-paren-token ")") a)
                                       #T))
                              (else (values #T (cons t a) m))))
                      '() #F)))
         (and modified? (reverse accum))))))

  (define/who upcase-booleans
    (make-transformation who
     (lambda (tokens)
       (let-values (((accum modified?)
                     (fold tokens
                      (lambda (t a m)
                        (define (trans lexeme)
                          (apply make-boolean-token lexeme (cdr (get-fields t))))
                        (if (boolean-token? t)
                          (let ((l (token-lexeme t)))
                            (cond ((string=? "#t" l)
                                   (values #T (cons (trans "#T") a) #T))
                                  ((string=? "#f" l)
                                   (values #T (cons (trans "#F") a) #T))
                                  (else (values #T (cons t a) m))))
                          (values #T (cons t a) m)))
                      '() #F)))
         (and modified? (reverse accum))))))

  (define/who hashbang-r6rs-first
    (make-transformation who
     (lambda (tokens)
       (define (it? l)
         (and (pair? l) (pair? (cdr l)) (pair? (cddr l))
              (hashbang-comment-token? (car l))
              #;(string=? "#!" (token-lexeme (car l)))
              (identifier-token? (cadr l))
              (string=? "r6rs" (token-lexeme (cadr l)))
              (whitespace-token? (caddr l))
              (char=? #\newline (string-ref (token-lexeme (caddr l)) 0))))
       #;(define (adjust-counts l)
           (R6RS-lexer (append-lexemes l)))
       (define (invalidate-counts l)
         (map (lambda (x)
                #;(assert (R6RS-token? x))
                ((record-constructor
                  (make-record-constructor-descriptor (record-rtd x) #F #F))
                 (token-lexeme x) #F #F #F))
              l))
       (define (separate l)
         (let loop ((l l) (pre '()))
           (if (null? l)
             (values #F #F)
             (if (it? l)
               (values (reverse pre)
                       (let ((s (token-lexeme (caddr l))))
                         (if (> (string-length s) 1)
                           (cons (make-whitespace-token
                                  (substring s 1 (string-length s)) #F #F #F)
                                 (cdddr l))
                           (cdddr l))))
               (loop (cdr l) (cons (car l) pre))))))
       (define (omit? l)
         (and-let* ((l (memp (not? atmosphere-token?) l)))
           (and (>= (length l) 2)
                (open-paren-token? (car l))
                (identifier-token? (cadr l))
                (member (token-lexeme (cadr l)) '("library" "import")))))
       (and (pair? tokens)
            (not (it? tokens))
            (cons* (make-hashbang-comment-token "#!" #F #F #F)
                   (make-identifier-token "r6rs" #F #F #F)
                   (make-whitespace-token "\n" #F #F #F)
                   (invalidate-counts
                    (let-values (((pre post) (separate tokens)))
                      (if (and post (omit? post))
                        (append pre post)
                        tokens))))))))

  (define (transform-file file . transformers)
    (define (output-port f)
      (open-file-output-port f
       (file-options no-fail) (buffer-mode block) (native-transcoder)))
    (let loop ((tokens (call-with-input-file file R6RS-lexer))
               (t transformers)
               (mod #F))
      (if (null? t)
        (and mod
             (call-with-port (output-port file)
               (lambda (fop)
                 (for-each (lambda (tok) (put-string fop (token-lexeme tok)))
                           tokens)
                 #T)))
        (let ((toks ((car t) tokens)))
          (loop (or toks tokens)
                (cdr t)
                (if toks #T mod))))))

  (define (transform-walk dir pred . transformers)
    (directory-walk
     (lambda (path dirs files syms)
       (for-each
        (lambda (f)
          (let ((f (path-join path f)))
            (when (pred f)
              (printf "~a ... " f)
              (if (apply transform-file f transformers)
                (display "wrote new contents\n")
                (display "not modified\n")))))
        files))
     dir))
)
