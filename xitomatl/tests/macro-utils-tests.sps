#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (srfi :78 lightweight-testing)
  (only (xitomatl exceptions) catch)
  (xitomatl macro-utils))

(define-syntax check-AV
  (syntax-rules ()
    ((_ expr)
     (check (catch ex ((else (assertion-violation? ex)))
              expr
              'unexpected-return)
            => #T))))

(define-syntax check-SV
  (syntax-rules ()
    ((_ msg form subform expr)
     (check (catch ex ((else (and (syntax-violation? ex)
                                  (message-condition? ex)
                                  (list (condition-message ex)
                                        (syntax->datum (syntax-violation-form ex))
                                        (syntax->datum (syntax-violation-subform ex))))))
              expr
              'unexpected-return)
            => '(msg form subform)))))

;; gen-temp
(check (identifier? (gen-temp)) => #T)
;; syntax->list
(check (list? (syntax->list #'(a b c d))) => #T)
(check (length (syntax->list #'(a b c d))) => 4)
(check (for-all identifier? (syntax->list #'(a b c d))) => #T)
(check (map syntax->datum (syntax->list #'(a b c d))) => '(a b c d))
(check-AV (syntax->list #'a))
(check-AV (syntax->list #'(a b c . d)))
;; with-syntax*
(let ((stx (with-syntax* ((x (gen-temp))
                          (y #'(foo x))) 
             #'(x y))))
  (check (eq? (car (syntax->list stx))
              (cadr (syntax->list (cadr (syntax->list stx))))) 
         => #T))
;; duplicate-id 
(check (duplicate-id '()) => #F)
(check (duplicate-id (list #'a #'b #'c #'d)) => #F)
(check (duplicate-id (list #'a #'b #'c #'d #'b)) (=> bound-identifier=?) #'b)
(check-AV (duplicate-id "oops"))
(check-AV (duplicate-id (list #'a #'b 'oops #'d)))
;; unique-ids? 
(check (unique-ids? '()) => #T)
(check (unique-ids? (list #'a #'b #'c #'d)) => #T)
(check (unique-ids? (list #'a #'b #'c #'a #'d)) => #F)
(check-AV (unique-ids? "oops"))
(check-AV (unique-ids? (list #'a #'b 'oops #'d)))
;; unique-ids?/raise 
(check (unique-ids?/raise '() #'orig) => #T)
(check (unique-ids?/raise (list #'a #'b #'c #'d) #'orig) => #T)
(check-SV "duplicate identifier" orig a
  (unique-ids?/raise (list #'a #'b #'c #'a #'d) #'orig))
(check-AV (unique-ids?/raise "oops"))
(check-AV (unique-ids?/raise (list #'a #'b 'oops #'d)))
;; formals-ok?/raise
(check (formals-ok?/raise #'() #'orig) => #T)
(check (formals-ok?/raise #'(a) #'orig) => #T)
(check (formals-ok?/raise #'(a b c) #'orig) => #T)
(check (formals-ok?/raise #'(a b c . r) #'orig) => #T)
(check (formals-ok?/raise #'r #'orig) => #T)
(check-SV "not an identifier" orig 1 
  (formals-ok?/raise #'(a b 1) #'orig))
(check-SV "not an identifier" orig 2
  (formals-ok?/raise #'(a b . 2) #'orig))
(check-SV "not an identifier" orig "oops"
  (formals-ok?/raise "oops" #'orig))
(check-SV "duplicate identifier" orig a
  (formals-ok?/raise #'(a b c a d) #'orig))
(check-SV "duplicate identifier" orig c
  (formals-ok?/raise #'(a b c . c) #'orig))
;; identifier-append 
(check (identifier-append #'here #'foo "-bar-" 'zab) (=> bound-identifier=?) #'foo-bar-zab)
(check-AV (identifier-append 'oops #'foo "-bar-" 'zab))
(check-AV (identifier-append #'here "" (string->symbol "")))
;; name=? 
(check (name=? #'foo 'foo "foo") => #T)
(check (name=? "foo" 'fooo #'foo) => #F)
;; identifier?/name=?
(check (identifier?/name=? #'blah 'blah) => #T)
(check (identifier?/name=? #'blah "blah") => #T)
(check (identifier?/name=? #'blah 'zab) => #F)
(check (identifier?/name=? 'blah 'blah) => #F)


(check-report)
