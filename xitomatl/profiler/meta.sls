#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

;; NOTE: Not currently thread-safe

(library (xitomatl profiler meta)
  (export
    def--/profiled
    make-make-profiled-proxy case-lambda/profiled--meta
    profiled-procedure?
    profiled-procedure-proc-obj
    profiled-procedure-source-code
    profiled-procedure-uses  ;; In reverse order: newest first
    procedure-use?
    procedure-use-start 
    procedure-use-stop 
    procedure-use-called
    procedure-use-returned
    profiled-procedures-HT
    reset-recorded-uses)
  (import
    (rnrs)
    (only (xitomatl define) define/AV)
    (only (srfi :39 parameters) make-parameter))
  
  (define-syntax def--/profiled
    (syntax-rules ()
      ((_ case-lambda--name lambda--name define--name make-profiled-proxy)
       (begin
         (define-syntax case-lambda--name
           (lambda (stx)
             (syntax-case stx ()
               ((_ . r)
                #`(case-lambda/profiled--meta '#,stx make-profiled-proxy . r)))))
         (define-syntax lambda--name
           (lambda (stx)
             (syntax-case stx ()
               ((_ . r)
                #`(case-lambda/profiled--meta '#,stx make-profiled-proxy r)))))
         (define-syntax define--name
           (lambda (stx)
             (syntax-case stx (case-lambda--name lambda--name)
               ((_ (n . formals) . body)
                (identifier? #'n)
                #`(define n 
                    (case-lambda/profiled--meta '#,stx make-profiled-proxy
                      (formals . body))))
               ((_ n (case-lambda--name . r))
                (identifier? #'n)
                #`(define n 
                    (case-lambda/profiled--meta '#,stx make-profiled-proxy . r)))
               ((_ n (lambda--name . r))
                (identifier? #'n)
                #`(define n 
                    (case-lambda/profiled--meta '#,stx make-profiled-proxy r)))
               ((_ . r)
                #'(define . r)))))))))
  
  (define (make-make-profiled-proxy current-info info-add info-sub)
    (lambda (proc)
      (define (profiled-proxy . args)
        (let ((enter-info-adj #F) (enter-info #F) (exit-info #F)
              (call-info-adj #F) (call-info #F) (return-info #F)
              (called (length args)) (returned #F))
          (dynamic-wind
           (lambda () 
             (set! enter-info-adj (current-info))
             (set! enter-info (current-info)))
           (lambda ()
             (call-with-values
              (lambda ()
                (set! call-info-adj (current-info))
                (set! call-info (current-info))
                (apply proc args))                         
              (lambda rv
                (set! return-info (current-info))
                (set! returned (length rv))
                (apply values rv))))
           (lambda ()
             (set! exit-info (current-info))
             (let-values (((i adj) 
                           (if called 
                             (values call-info (info-sub call-info call-info-adj))
                             (values enter-info (info-sub enter-info enter-info-adj)))))
               (let ((start (info-add i adj))
                     (stop (info-sub (if returned return-info exit-info) adj)))
                 (record-procedure-use profiled-proxy start stop called returned)))
             ;; Clean-up in case a continuation in proc was captured.
             (set! enter-info-adj #F)
             (set! enter-info #F) 
             (set! exit-info #F)
             (set! call-info-adj #F) 
             (set! call-info #F) 
             (set! return-info #F)
             (set! called #F)
             (set! returned #F)))))
      profiled-proxy))
  
  (define-syntax case-lambda/profiled--meta
    (syntax-rules ()
      ((_ source-code make-profiled-proxy (formals . body) ...)
       (let ((profiled-proxy
              (make-profiled-proxy (case-lambda (formals . body) ...))))
         (register-procedure profiled-proxy source-code)
         profiled-proxy))))
    
  (define-record-type profiled-procedure
    (fields proc-obj source-code (mutable uses)))
  
  (define-record-type procedure-use
    (fields start stop called returned))
  
  (define/AV profiled-procedures-HT 
    (make-parameter (make-eq-hashtable)
                    (lambda (x) 
                      (if (and (hashtable? x)
                               (eq? eq? (hashtable-equivalence-function x)))
                        x
                        (AV "not an eq-hashtable" x)))))
  
  (define (register-procedure proc source-code)
    (hashtable-set! (profiled-procedures-HT) proc 
      (make-profiled-procedure proc source-code '())))
  
  (define (record-procedure-use proc start stop called returned)
    (let ((pp (hashtable-ref (profiled-procedures-HT) proc #F)))
      (profiled-procedure-uses-set! pp 
        (cons (make-procedure-use start stop called returned) 
              (profiled-procedure-uses pp)))))  
  
  (define (reset-recorded-uses)
    (let-values (((keys vals) (hashtable-entries (profiled-procedures-HT))))
      (vector-for-each 
        (lambda (pp)
          (profiled-procedure-uses-set! pp '())) 
        vals)))  
)
