#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (rnrs eval)
  (srfi :78 lightweight-testing)
  (only (xitomatl exceptions) catch)
  (for (only (xitomatl file-system base) current-directory) expand)
  (xitomatl include))

(define-syntax check-include-ex
  (syntax-rules ()
    ((_ main-ex? who msg expr)
     (check (catch ex ((else (and (main-ex? ex)
                                  (who-condition? ex)
                                  (message-condition? ex)
                                  (list (condition-who ex) 
                                        (condition-message ex)))))
              (eval 'expr (environment '(rnrs) '(xitomatl include)))
              'unexpected-return)
            => '(who msg)))))

(define-syntax check-include-error
  (syntax-rules ()
    ((_ who msg expr)
     (check-include-ex error? who msg expr))))

;; include
(check (let () 
         (include "file-a")
         (+ x y))
       => 3)
(check (let () 
         (define x 1)
         (define y 2)
         (include "file-b"))
       => 3)
(check (let () 
         (include "file-a")
         (include "file-b"))
       => 3)
(let-syntax ((ae (begin (assert (file-exists? "file-c")) (lambda (_) #F))))
  (ae)
  (check-include-error include/lexical-context "error while trying to include" 
    (include "file-c")))
(check-include-error include/lexical-context "error while trying to include" 
  (include "doesnt-exist"))
;; include/lexical-context
(define-syntax s
  (lambda (stx)
    (syntax-case stx ()
      ((ctxt fn)
       #'(include/lexical-context ctxt fn)))))
(check (let () 
         (s "file-a")
         (+ x y))
       => 3)
(check (let () 
         (define x 1)
         (define y 2)
         (s "file-b"))
       => 3)
(check (let () 
         (s "file-a")
         (s "file-b"))
       => 3)
(check (let ((begin 'different)
             (x 1)
             (y 2))
         (s "file-b"))
       => 3)
(check-include-ex syntax-violation? include/lexical-context "not an identifier" 
  (include/lexical-context "oops" "ignored"))
(check-include-ex syntax-violation? include/lexical-context "not a path" 
  (include/lexical-context here oops))
;; include/resolve
(check (let ()
         (define-syntax cd (begin (current-directory "/tmp") (lambda (_) #'(begin))))
         (cd)
         (include/resolve ("xitomatl" "tests" "include") "file-a")
         (+ x y))
       => 3)
(check (let ()
         (define-syntax cd (begin (current-directory "/tmp") (lambda (_) #'(begin))))
         (cd)
         (define x 1)
         (define y 2)
         (include/resolve ("xitomatl" "tests" "include") "file-b"))
       => 3)
(check (let ()
         (define-syntax cd (begin (current-directory "/tmp") (lambda (_) #'(begin))))
         (cd)
         (include/resolve ("xitomatl" "tests" "include") "file-a")
         (include/resolve ("xitomatl" "tests" "include") "file-b"))
       => 3)
(check-include-error include/lexical-context "error while trying to include" 
  (include/resolve ("xitomatl" "tests" "include") "file-c"))
(check-include-error include/resolve "cannot find file in search paths"
  (include/resolve ("no" "where") "doesnt-exist"))


(check-report)
