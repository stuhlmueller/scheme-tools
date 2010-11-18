#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (only (rnrs eval) environment)
  (srfi :78 lightweight-testing)
  (only (xitomatl exceptions) catch)
  (only (xitomatl strings) string-end=?)
  (xitomatl repl))

(define (string-repl in-str env)
  (let ((in (open-string-input-port in-str)))
    (let-values (((out out-get) (open-string-output-port))
                 ((err err-get) (open-string-output-port)))
      (repl in out err env)
      (values (out-get) (err-get)))))

(define env (environment '(rnrs)))

(define (_check-repl in out err)
  (let-values (((o e) (string-repl in env)))
    (cond ((and (string? out) (string? err))
           (check (list o e) => (list out err)))
          ((and (string? out) (procedure? err))
           (check (list o (err e)) => (list out #T)))          
          ((and (procedure? out) (string? err))
           (check (list (out o) e) => (list #T err)))                    
          ((and (procedure? out) (procedure? err))
           (check (list (out o) (err e)) => '(#T #T)))
          (else (assert #F)))))

(define-syntax check-repl
  (lambda (stx)
    (syntax-case stx (=>)
      ((_ in => out err)
       #'(_check-repl in out err)))))

(check-repl "(+ 1 2)"
            => "> 3\n> \n" "")
(check-repl "(begin (write 'zab)(display \"foo\" (current-error-port))(values))"
            => "> zab> \n" "foo")
(check-repl "(read (current-input-port)) foo(read (current-input-port))bar "
            => "> foo\n> bar\n> \n" "")
(check-repl "(begin (raise-continuable (make-warning)) 'continued)" 
            => "> continued\n> \n" (lambda (es) (positive? (string-length es))))
(check-repl "'foo -oops 'bar"
            => "> foo\n> \n" (lambda (es) (string-end=? es "\nQuiting REPL.\n")))
(check 
 (catch ex (((assertion-violation? ex)))
   (_check-repl "(begin (close-port (current-error-port)) (raise 'try-to-print))" "" ""))
 => #T)


(check-report)
