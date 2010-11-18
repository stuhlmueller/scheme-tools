#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (for (except (rnrs base) define)
       (meta 0))
  (for (prefix (only (rnrs base) define) rnrs:)
       (meta 0))
  (for (only (rnrs base) lambda)
       (meta 1))
  (for (only (rnrs io simple) display)
       (meta 0))
  (for (except (rnrs syntax-case) syntax)
       (meta 1))
  (for (prefix (only (rnrs syntax-case) syntax) rnrs:)
       (meta 0) (meta 1))
  (for (only (xitomatl macro-utils) free-identifier-bound?)
       (meta 1))
  (for (xitomatl macro-utils fib ctxt)
       (meta 0))
  (for (xitomatl macro-utils fib p-ctxt)
       (meta 0)))

(define-syntax test
  (lambda (stx)
    (syntax-case stx ()
      ((_ id bool)
       (with-syntax ((bound? (free-identifier-bound? (rnrs:syntax id))))
         (rnrs:syntax
          (begin
            (display 'id) (display " => ")
            (display (if bound? "bound " "unbound "))
            (display (if (boolean=? bound? bool) "(pass)\n" "(FAIL)\n")))))))))

(test list #T)
(test foobar #F)
(let ((foobar 1))
  (test foobar #T))

(test rnrs:define #T)
(test rnrs:syntax #T)
(test ctxt #T)
(test p-ctxt #T)

(test define #F)
(test syntax #F)

(let ((define 1)
      (syntax 1))
  (test define #T)
  (test syntax #T))

(test p-define #F)
(test p-syntax #F)

(let ((p-define 1)
      (p-syntax 1))
  (test p-define #T)
  (test p-syntax #T))
