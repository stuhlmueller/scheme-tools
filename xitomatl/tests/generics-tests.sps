#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (rnrs eval)
  (xitomatl generics)
  (srfi :78 lightweight-testing)
  (only (xitomatl define) define-values))

(define-syntax check-AV-who-msg
  (syntax-rules ()
    ((_ who msg expr)
     (check (guard (ex (else (and (assertion-violation? ex)
                                  (message-condition? ex)
                                  (who-condition? ex)
                                  (list (condition-who ex) 
                                        (condition-message ex)))))
              expr
              'unexpected-return)
            => '(who msg)))))

(define-syntax check-no-spec
  (syntax-rules ()
    ((_ who expr)
     (check-AV-who-msg who "no specialization" expr))))

(define-syntax check-SV
  (syntax-rules ()
    ((_ expr)
     (check (guard (ex (else (syntax-violation? ex)))
              (eval 'expr (environment '(rnrs) '(xitomatl generics)))
              'unexpected-return)
            => #T))))

(define invalid-preds-spec-msg "invalid predicates specification")

(define-generic/temporal g0)
(check (procedure? g0) => #T)
(check-no-spec g0 (g0))
(g0-specialize! '() (lambda () 1))
(check (g0) => 1)
(check-no-spec g0 (g0 'a))
(g0-specialize! (list symbol?) symbol->string)
(check (g0 'a) => "a")
(check-no-spec g0 (g0 'a 'b))
(g0-specialize! (list symbol? symbol?) symbol=?)
(check (g0 'a 'b) => #F)
(check (g0 'a 'a) => #T)
(check-no-spec g0 (g0 'a 3))
(g0-specialize! (list (lambda (x) (or (symbol? x) (string? x))) number?) 
  (lambda (s n) (make-vector n s)))
(check (g0 'a 3) => '#(a a a))
(check (g0 "a" 4) => '#("a" "a" "a" "a"))
(check-no-spec g0 (g0 "a" "3"))
(check-no-spec g0 (g0 #\c))
(check-no-spec g0 (g0 #\a #\b #\c))
;; required arguments and rest arguments list
(g0-specialize! (cons* char? (lambda r (for-all char-alphabetic? r)))
  (lambda (c . r) (apply string c r)))
(check (g0 #\1) => "1")
(check (g0 #\1 #\b #\c #\d) => "1bcd")
(check-no-spec g0 (g0 #\1 #\2))
(check-no-spec g0 (g0 #\1 #\2 #\3 #\4))
;; no required, rest arguments list
(g0-specialize! (lambda args #T) (lambda args (reverse args)))
;; specializations have precedence according to their order of being added
(check (g0) => 1)
(check (g0 1 2 3) => '(3 2 1))
(check (g0 's) => "s")
(check (g0 's 's) => #T)
(check (g0 "x" 'y) => '(y "x"))
(check (g0 "x" 2) => '#("x" "x"))
(check (g0 #\1 #\2 #\3 #\4) => '(#\4 #\3 #\2 #\1))
(check (g0 #\1) => "1")
(check (g0 #\1 #\z) => "1z")
(check (g0 '(x)) => '((x)))
(check (g0 's 3 's #\c) => '(#\c s 3 s))
;; misuse errors
(check-AV-who-msg make-generic "argument check failed"
  (make-generic 1 #\c))
(check-AV-who-msg reconfigure/temporal "argument check failed" 
  (reconfigure/temporal '() 'oops values))
(check-AV-who-msg reconfigure/temporal "argument check failed" 
  (reconfigure/temporal '() (list 'oops) values))
(check-AV-who-msg reconfigure/temporal "argument check failed" 
  (reconfigure/temporal '() (cons* char? number? 'oops) values))
(check-AV-who-msg reconfigure/temporal "argument check failed"
  (reconfigure/temporal '() (list number?) 'oops))
(define-values (g1 g1-specs) 
  (make-generic))
(check-no-spec "a generic" (g1))
(check-AV-who-msg "a generic specializer" "invalid specializations value"
  (g1-specs 'oops))
(check-AV-who-msg "a generic specializer" "invalid specializations value"
  (g1-specs (list (list 'oops values))))
(check-AV-who-msg "a generic specializer" "invalid specializations value"
  (g1-specs (list (list (list 'oops) values))))
(check-AV-who-msg "a generic specializer" "invalid specializations value"
  (g1-specs (list (list (cons* char? number? 'oops) values))))
(check-AV-who-msg "a generic specializer" "invalid specializations value"
  (g1-specs (list (list (list number?) 'oops))))
(let ()
  (define-values (foo foo-specs) 
    (make-generic 'foo 'foo-specs))
  (check-no-spec foo (foo))
  (check-AV-who-msg foo-specs "invalid specializations value"
    (foo-specs 'oops)))
;; define-generic/temporal syntax
(define-generic/temporal g2
  (() 1)
  (((x symbol?))
   (symbol->string x))
  (((x symbol?) (y symbol?))
   (symbol=? x y))
  (((x (lambda (x) (or (symbol? x) (string? x)))) (y number?))
   (make-vector y x))
  (((x char?) . #(y (lambda r (for-all char-alphabetic? r))))
   (apply string x y))
  (#(x (lambda a #T))
   (reverse x)))
(check (g2) => 1)
(check (g2 'a) => "a")
(check (g2 'a 'b) => #F)
(check (g2 'a 'a) => #T)
(check (g2 'a 3) => '#(a a a))
(check (g2 "a" 4) => '#("a" "a" "a" "a"))
(check (g2 #\1) => "1")
(check (g2 #\1 #\b #\c #\d) => "1bcd")
(check (g2 1 2 3) => '(3 2 1))
(check (g2 's) => "s")
(check (g2 's 's) => #T)
(check (g2 "x" 'y) => '(y "x"))
(check (g2 "x" 2) => '#("x" "x"))
(check (g2 #\1 #\2 #\3 #\4) => '(#\4 #\3 #\2 #\1))
(check (g2 #\1) => "1")
(check (g2 #\1 #\z) => "1z")
(check (g2 '(x)) => '((x)))
(check (g2 's 3 's #\c) => '(#\c s 3 s))
(check-AV-who-msg reconfigure/temporal "argument check failed" 
  (let ()
    (define-generic/temporal g1 
      (((a 'oops)) (values)))
    g1))
(check-AV-who-msg reconfigure/temporal "argument check failed" 
  (let ()
    (define-generic/temporal g1 
      (#(a 'oops) (values)))
    g1))
(check-AV-who-msg reconfigure/temporal "argument check failed" 
  (let ()
    (define-generic/temporal g1 
      (((z char?) . #(a 'oops)) (values)))
    g1))
(check-SV (let ()
            (define-generic/temporal g1 
              ((oops) (values)))
            g1))
(check-SV (let ()
            (define-generic/temporal g1 
              ((oops oops2) (values)))
            g1))
(check-SV (let ()
            (define-generic/temporal g1 
              (oops (values)))
            g1))
(check-SV (let ()
            (define-generic/temporal g1 
              (#(oops) (values)))
            g1))
(check-SV (let ()
            (define-generic/temporal g1 
              (((1 char?)) (values)))
            g1))
(check-SV (let ()
            (define-generic/temporal g1 
              (((a null?) . #(1 char?)) (values)))
            g1))
(check-SV (let ()
            (define-generic/temporal g1 
              (#(1 char?) (values)))
            g1))
(check-SV (let ()
            (define-generic/temporal g1 
              ((#(x char?)) (values)))
            g1))
;; define-generic/temporal does not break internal define contexts
(check (let ()
         (define-generic/temporal g
           (((a (lambda (a) #T))) a))
         (define x 'okay)
         (g x))
       => 'okay)


(check-report)
