#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl repl)
  (export
    repl)
  (import 
    (except (rnrs) current-input-port current-output-port current-error-port)
    (rnrs eval)
    (only (xitomatl define) define/?)
    (only (xitomatl exceptions) print-exception catch reraise)
    (only (srfi :39 parameters) parameterize)
    (only (xitomatl common) pretty-print)
    (only (xitomatl ports) textual-input-port? textual-output-port?)
    (only (xitomatl environments) environment?)
    (xitomatl repl compat))
  
  
  (define/? repl 
    (case-lambda/?
      ((in out err env)
       (repl in out err env (lambda () "> ")))
      (((in textual-input-port?) 
        (out textual-output-port?)
        (err textual-output-port?)
        (env environment?)
        (prompt procedure?))
       (define (print-ex ex)
         (flush-output-port out)
         (print-exception ex err)
         (flush-output-port err))
       (let loop ()
         (display (prompt) out)
         (flush-output-port out)
         (let ((x (catch ex (((lexical-violation? ex)
                              (print-ex ex)
                              (display "\nQuiting REPL.\n" err)
                              (flush-output-port err)
                              (eof-object)))
                    (read in))))
           (cond
             ((eof-object? x) 
              (newline out)
              (flush-output-port out)
              (values))
             (else
              (call/cc
               (lambda (k)
                 (call-with-values
                  (lambda () 
                    (with-exception-handler
                      (lambda (ex)
                        (if (non-continuable-violation? ex)
                          (begin (print-ex ex) 
                                 (k))
                          (reraise ex)))
                      (lambda ()
                        (with-exception-handler
                          (lambda (ex)
                            (print-ex ex)
                            (when (serious-condition? ex)
                              (k))
                            (values))
                          (lambda ()
                            (parameterize ((current-input-port in)
                                           (current-output-port out)
                                           (current-error-port err))
                              (eval x env)))))))
                  (lambda vals
                    (for-each (lambda (v) (pretty-print v out))
                              vals)
                    (flush-output-port out)))))
              (loop))))))))
)
