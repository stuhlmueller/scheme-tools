#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl IU-match extras)
  (export
    match-lambda match-lambda/lexical-context
    match-lambda* match-lambda*/lexical-context
    match-let match-let/lexical-context
    match-let* match-let*/lexical-context    
    match-letrec match-letrec/lexical-context
    match-define match-define/lexical-context
    ;; All of (xitomatl IU-match)
    match trace-match 
    match/lexical-context trace-match/lexical-context
    match-equality-test
    guard ... quasiquote unquote unquote-splicing)
  (import
    (rnrs)
    (xitomatl IU-match)
    (for (xitomatl IU-match macro-helpers) expand))
  
  (define-syntax match-lambda/lexical-context
    (lambda (stx)
      (syntax-case stx ()
        ((_ ctxt clause ...)
         #'(lambda (x) (match/lexical-context ctxt x clause ...))))))
  
  (define-syntax match-lambda
    (lambda (stx)
      (syntax-case stx ()
        ((ctxt clause ...)
         #'(match-lambda/lexical-context ctxt clause ...)))))
  
  (define-syntax match-lambda*/lexical-context
    (lambda (stx)
      (syntax-case stx ()
        ((_ ctxt clause ...)
         #'(lambda x (match/lexical-context ctxt x clause ...))))))
  
  (define-syntax match-lambda*
    (lambda (stx)
      (syntax-case stx ()
        ((ctxt clause ...)
         #'(match-lambda*/lexical-context ctxt clause ...)))))
  
  (define-syntax match-let/lexical-context
    (lambda (stx)
      (define (check-patterns pat*) 
        (check-ids/prevent-dups-across stx #'match-let pat*))
      (syntax-case stx ()
        ((_ ctxt named () body0 body* ...)
         #'(let named () body0 body* ...))
        ((_ ctxt () body0 body* ...)
         #'(let () body0 body* ...))
        ((_ ctxt named ((pat expr)) body0 body* ...)
         (check-patterns (list #'pat))
         #'(letrec ((named (lambda (t) (match/lexical-context ctxt t (pat body0 body* ...)))))
             (named expr)))
        ((_ ctxt named ((pat* expr*) ...) body0 body* ...)
         (check-patterns #'(pat* ...))
         (with-syntax (((t* ...) (generate-temporaries #'(pat* ...))))
           #'(letrec ((named (lambda (t* ...) 
                               (match/lexical-context ctxt (vector t* ...) 
                                 (#(pat* ...) body0 body* ...)))))
               (named expr* ...))))
        ((_ ctxt ((pat expr)) body0 body* ...)
         (check-patterns (list #'pat))
         #'(match/lexical-context ctxt expr (pat body0 body* ...)))
        ((_ ctxt ((pat* expr*) ...) body0 body* ...)
         (check-patterns #'(pat* ...))
         #'(match/lexical-context ctxt (vector expr* ...) (#(pat* ...) body0 body* ...))))))
  
  (define-syntax match-let
    (lambda (stx)
      (syntax-case stx ()
        ((_ named () body0 body* ...)
         (identifier? #'named)
         #'(let named () body0 body* ...))
        ((_ () body0 body* ...)
         #'(let () body0 body* ...))
        ((ctxt named ((pat* expr*) ...) body0 body* ...)
         (identifier? #'named)
         #'(match-let/lexical-context ctxt named ((pat* expr*) ...) body0 body* ...))
        ((ctxt ((pat* expr*) ...) body0 body* ...)
         #'(match-let/lexical-context ctxt ((pat* expr*) ...) body0 body* ...)))))
  
  (define-syntax match-let*/lexical-context
    (lambda (stx)
      (define (check-patterns pat*)
        (check-ids/dups-okay stx #'match-let* pat*))
      (define (nest-matching s)
        (syntax-case s ()
          ((ctxt () body0 body* ...)
           #'(let () body0 body* ...))
          ((ctxt ((pat0 expr0) (pat* expr*) ...) body0 body* ...)
           #`(match/lexical-context ctxt expr0
               (pat0 #,(nest-matching #'(ctxt ((pat* expr*) ...) body0 body* ...)))))))
      (syntax-case stx ()
        ((_ ctxt () body0 body* ...)
         #'(let () body0 body* ...))
        ((_ ctxt ((pat* expr*) ...) body0 body* ...)
         (check-patterns #'(pat* ...))
         (nest-matching #'(ctxt ((pat* expr*) ...) body0 body* ...))))))
  
  (define-syntax match-let*
    (lambda (stx)
      (syntax-case stx ()
        ((_ () body0 body* ...)
         #'(let () body0 body* ...))
        ((ctxt ((pat* expr*) ...) body0 body* ...)
         #'(match-let*/lexical-context ctxt ((pat* expr*) ...) body0 body* ...)))))
  
  (define-syntax match-letrec/lexical-context
    (lambda (stx)
      (define (check-patterns pat*) 
        (check-ids/prevent-dups-across stx #'match-letrec pat*))
      (syntax-case stx ()
        ((_ ctxt () body0 body* ...)
         #'(let () body0 body* ...))
        ((_ ctxt ((pat* expr*) ...) body0 body* ...)
         (check-patterns #'(pat* ...))
         #'(let ()
             (match-define/lexical-context ctxt #(pat* ...) (vector expr* ...))
             body0 body* ...)))))
  
  (define-syntax match-letrec
    (lambda (stx)
      (syntax-case stx ()
        ((_ () body0 body* ...)
         #'(let () body0 body* ...))
        ((ctxt ((pat* expr*) ...) body0 body* ...)
         #'(match-letrec/lexical-context ctxt ((pat* expr*) ...) body0 body* ...)))))
  
  (define-syntax match-define/lexical-context
    (lambda (stx)
      (syntax-case stx ()
        ((_ ctxt pat expr)
         (with-syntax (((id* ...) (find-ids/prevent-auto-recur stx #'match-define #'pat)))
           (with-syntax (((t* ...) (generate-temporaries #'(id* ...))))
             #'(begin
                 (define t*) ...
                 (define dummy
                   (match/lexical-context ctxt expr (pat (set! t* id*) ... #F)))
                 (define id* 
                   (let ((v t*)) (set! t* #F) v)) 
                 ...)))))))
  
  (define-syntax match-define
    (lambda (stx)
      (syntax-case stx ()
        ((ctxt pat expr)
         #'(match-define/lexical-context ctxt pat expr)))))
  
)
