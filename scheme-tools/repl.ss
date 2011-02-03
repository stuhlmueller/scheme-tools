#!r6rs

;; Provides a repl that can be called at any point within a file and
;; that has access to objects that are passed into it and to exports
;; of the library it is invoked in.

(library

 (scheme-tools repl)

 (export repl)

 (import (rnrs)         
         (scheme-tools implementation-specific)
         (scheme-tools readable-scheme)
         (scheme-tools external))

 (define-syntax repl
   (syntax-rules ()
     ((repl objs ...)
      (let ([env (interaction-environment)])
        (call/cc (lambda (top) (obj-repl env top '(objs ...) (list objs ...))))))))
 
 (define (obj-repl env top obj-names objs)
   (pe "\nscm> ")
   (let ((input (read)))
     (cond
      ((equal? input 'q) (begin (pe "exiting repl.\n") (top (void))))
      ((equal? input 'qq) (begin (pe "exiting scheme.\n") (exit)))
      (else
       (with-exception-handler
        (lambda (exn)
          (pe (format "exception:\n~a" exn))
          (obj-repl env top obj-names objs))
        (lambda ()
          (let* ([proc (eval `(lambda ,obj-names ,input) env)]
                 [val (apply proc objs)])
            (when (not (equal? val (void)))
                  (write val))
            (obj-repl env top obj-names objs))))))))

 )