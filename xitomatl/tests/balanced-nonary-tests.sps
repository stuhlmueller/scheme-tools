#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (srfi :78 lightweight-testing)
  (only (srfi :39 parameters) parameterize)
  (only (xitomatl exceptions) catch)
  (xitomatl numeral-system balanced-nonary))

(define-syntax check-AV
  (syntax-rules ()
    ((_ expr)
     (check (catch ex ((else (assertion-violation? ex)))
              expr
              'unexpected-return)
            => #T))))

(check-set-mode! 'report-failed)


(check (balanced-nonary-digits)
       => '((#\D . -4) (#\C . -3) (#\B . -2) (#\A . -1)
            (#\0 . 0) (#\1 . 1) (#\2 . 2) (#\3 . 3) (#\4 . 4)))
(parameterize
    ((balanced-nonary-digits
      '((#\➍ . -4) (#\➌ . -3) (#\➋ . -2) (#\➊ . -1)
        (#\◯ . 0) (#\➀ . 1) (#\➁ . 2) (#\➂ . 3) (#\➃ . 4))))
  (check (balanced-nonary-digits)
         => '((#\➍ . -4) (#\➌ . -3) (#\➋ . -2) (#\➊ . -1)
              (#\◯ . 0) (#\➀ . 1) (#\➁ . 2) (#\➂ . 3) (#\➃ . 4))))
(let-syntax ((test (syntax-rules ()
                     ((_ expr)
                      (check-AV (balanced-nonary-digits expr))))))
  (test "oops")
  (test '((#\C . -3) (#\B . -2) (#\A . -1)
          (#\0 . 0) (#\1 . 1) (#\2 . 2) (#\3 . 3) (#\4 . 4)))
  (test '(1 2 3 4 5 6 7 8 9))
  (test '((#\D . -4) (#\C . -3) (#\B . -2) (#\A . -1)
          (#\0 . 0) (#\1 . 1) (oops . 2) (#\3 . 3) (#\4 . 4)))
  (test '((#\D . -4) (#\C . -3) (#\B . -2) (#\A . -1)
          (#\0 . 0) (#\1 . 1) (#\D . 2) (#\3 . 3) (#\4 . 4)))
  (test '((#\D . -4) (#\C . -3) (#\B . -2) (#\A . -1)
          (#\0 . 0) (#\1 . 1) (#\2 . oops) (#\3 . 3) (#\4 . 4)))
  (test '((#\D . -4) (#\C . -3) (#\B . -2) (#\A . -1)
          (#\0 . 0) (#\1 . 1) (#\2 . -2) (#\3 . 3) (#\4 . 4))))


(define (test-permutations len)
  (define (test s i)
    (check (balanced-nonary->integer s) => i))
  (define digits
    (map car (list-sort (lambda (x y) (< (cdr x) (cdr y)))
                        (balanced-nonary-digits))))
  (define (permute l a i)
    (if (positive? l)
      (let loop ((d digits) (i i))
        (if (null? d)
          i
          (loop (cdr d)
                (permute (- l 1)
                         (cons (car d) a)
                         i))))
      (begin (test (list->string (reverse a)) i)
             (+ 1 i))))
  (permute len '() (- (/ (- (expt 9 len) 1) 2))))

(test-permutations 7)


(define (test-range from to)
  (do ((i from (+ 1 i)))
      ((> i to))
    (check (balanced-nonary->integer (integer->balanced-nonary i))
           => i)))

(test-range -654321 765432)


(check-report)
