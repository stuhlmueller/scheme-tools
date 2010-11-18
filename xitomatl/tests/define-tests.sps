#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rename (rnrs) (define rnrs:define) (define-syntax rnrs:define-syntax))
  (rnrs eval)
  (xitomatl define)
  (xitomatl define extras)
  (xitomatl conditions)
  (srfi :78 lightweight-testing))
  
(rnrs:define-syntax check-syntax-error
  (syntax-rules ()
    ((_ expr)
     (check (guard (ex ((syntax-violation? ex) #T)
                       (else `(dont-know: ,ex)))
              (eval '(let () expr) 
                    (environment '(except (rnrs) define define-syntax) 
                                 '(xitomatl define) 
                                 '(xitomatl define extras))) 
              '(succeeded: expr))
            => #T))))

(rnrs:define-syntax check-AV
  (syntax-rules ()
    ((_ expr)
     (check (guard (ex ((assertion-violation? ex) #T)
                       (else `(dont-know: ,ex)))
              (let () expr '(succeeded: expr)))
            => #T))))

(rnrs:define-syntax check-AV/msg
  (lambda (stx)
    (syntax-case stx ()
      ((_ msg expr)
       (string? (syntax->datum #'msg))
       #'(check (guard (ex ((and (assertion-violation? ex)
                                 (message-condition? ex))
                            (condition-message ex))
                           (else `(dont-know: ,ex)))
                  (let () expr '(succeeded: expr)))
                => msg)))))

(rnrs:define-syntax check-AV/who
  (lambda (stx)
    (syntax-case stx ()
      ((_ who expr)
       (symbol? (syntax->datum #'who))
       #'(check (guard (ex ((and (assertion-violation? ex)
                                 (who-condition? ex))
                            (condition-who ex))
                           (else `(dont-know: ,ex)))
                  (let () expr '(succeeded: expr)))
                => 'who)))))

(rnrs:define-syntax check-AV/msg/AN
  (lambda (stx)
    (syntax-case stx ()
      ((_ msg an expr)
       (and (string? (syntax->datum #'msg))
            (identifier? #'an))
       #'(check (guard (ex ((and (assertion-violation? ex)
                                 (message-condition? ex)
                                 (argument-name-condition? ex))
                            (list (condition-message ex)
                                  (condition-argument-name ex)))
                           (else `(dont-know: ,ex)))
                  (let () expr '(succeeded: expr)))
                => '(msg an))))))

(rnrs:define-syntax check-no-error
  (syntax-rules ()
    ((_ expr)
     (check (guard (ex (#T #F))
              (let () expr #T))
            => #T))))

;;;; define-values

(define-values (a) 1)
(check a => 1)
(check-no-error (define-values () (values)))
(define-values (b c) (values 2 3))
(check (list b c) => '(2 3))
(define-values (d e f g h i j k l m n o p q r s t u v w x y z) 
  (values 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26))
(check (list d e f g h i j k l m n o p q r s t u v w x y z)
       => '(4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26))
(define-values all0 (values))
(check all0 => '())
(define-values all1 (values 1))
(check all1 => '(1))
(define-values all2 (values 1 2 3 4))
(check all2 => '(1 2 3 4))
(define-values (aa . r0) (values 1 2 3 4))
(check aa => 1)
(check r0 => '(2 3 4))
(define-values (bb cc . r1) (values 1 2))
(check bb => 1)
(check cc => 2)
(check r1 => '())
(check (let ()
         (define a 1)
         (define-values (b c) (values 2 3))
         (define d 4)
         (+ a b c d))
       => 10)
(check-AV
  (define-values (a) (values 1 2)))
(check-AV
  (define-values (a b c) (values 1 2)))
(check-AV
  (define-values (a b c d e f g) (values 1 2 3)))
(check-AV
  (define-values () (values 1 2)))
(check-AV
  (define-values () (values 1)))
(check-syntax-error
 (define-values (a b a) (values 1 2 3)))

;;;; currying

(define (fn x) x)
(check (fn 678) => 678)
(define ((fc a) b) (- b a))
(check ((fc 5) 9) => 4)
(define ((((fc2 a) b) c) d) (+ a b c d))
(check ((((fc2 1) 2) 3) 4) => 10)

;;;; defining macro transformers

(define-syntax M
  (lambda (stx)
    (syntax-case stx () ((_ x) #'(reverse x)))))
(check (M '(a b c)) => '(c b a))
(define-syntax (M2 stx)
  (syntax-case stx () ((_ x) #'(list x))))
(check (M2 'zzz) => '(zzz))

;;;; define/who

(define/who (w0 x y . a) (list x y a who))
(check (w0 1 2 3 4) => '(1 2 (3 4) w0))
(define/who w1 (case-lambda (() 'bad) ((a) (list a who))))
(check (w1 1) => '(1 w1))
(define/who w2 (list who who))
(check w2 => '(w2 w2))

;;;; define/AV

(define/AV f0 (case-lambda (() (AV "oops")) (r r)))
(check (f0 1 2 3) => '(1 2 3))
(define/AV f1 (case-lambda (() (AV "oops1" 'ign)) (ign #F)))
(check-AV/msg "oops1" (f1))
(define/AV f2 (lambda (a) (list->string (reverse (string->list a)))))
(check (f2 "asdf") => "fdsa")
(define/AV f3 (lambda () (AV "oops2")))
(check-AV/msg "oops2" (f3))
(let ()
  (define/AV (f x y . zs) 
    (when (null? zs) (AV "zs null" zs))
    (apply * x y zs))
  (check (f 1 2 3 4 5) => 120))
(check-AV/msg "oops3"
 (let ()
   (define/AV (f) (AV "oops3" 'ign 'ign 'ign))
   (f)))
(check-AV/who f
  (let ()
    (define/AV f (lambda () (AV "oops")))
    (f)))

;;;; define/?

(define/? f4 (case-lambda/? (() 'a) ((x) 'b)))
(check (f4) => 'a)
(define/? f5 (case-lambda/? (() 'a) ((x) 'b)))
(check (f5 1) => 'b)
(define/? f6 (case-lambda/? (() 'a) (((x integer?)) 'b)))
(check (f6 1) => 'b)
(define/? f7 (case-lambda/? (() 'a) (r 'b)))
(check (f7 1) => 'b)
(define/? f8 (case-lambda/? (rest (reverse rest)) (((x integer?)) 'b)))
(check (f8 1 2 3) => '(3 2 1))
(define/? f9 (case-lambda/? (#(rest list?) (reverse rest)) (((x integer?)) 'b)))
(check (f9 1 2 3) => '(3 2 1))
(define/? f10 (case-lambda/? 
                ((x (y string?) z . #(r (lambda (r) (for-all number? r)))) 
                 (cons* x y z r)) 
                ((x) 'b)))
(check (f10 'x "yy" #\z 1 -56.3 67/902) => '(x "yy" #\z 1 -56.3 67/902))
(define/? f11 (case-lambda/? 
                (((x symbol?) (y string?) (z char?) 
                  . #(r (lambda (r) (for-all number? r)))) 
                 (cons* x y z r)) 
                ((x) 'b)))
(check (f11 'x "yy" #\z 1 -56.3 67/902) => '(x "yy" #\z 1 -56.3 67/902))
(check-syntax-error (define/? f (case-lambda/? (() 'a) (((x)) 'b))))
(check-syntax-error (define/? f (case-lambda/? (() 'a) (((x integer? oops)) 'b))))
(check-syntax-error (define/? f (case-lambda/? (#() (reverse rest)) (((x integer?)) 'b))))
(check-syntax-error 
 (define/? f (case-lambda/? (#((rest oops)) (reverse rest)) (((x integer?)) 'b))))
(check-syntax-error 
 (define/? f (case-lambda/? (#(rest list? oops) (reverse rest)) (((x integer?)) 'b))))
(check-syntax-error 
 (define/? f (case-lambda/?
               ((x (y string?) z . #((r (lambda (r) (for-all number? r))))) 
                (cons* x y z r)) 
               ((x) 'b))))
(define/? f12 (case-lambda/? (() 'a) (((x string?)) 'b)))
(check-AV/msg/AN "argument check failed" x (f12 1))
(define/? f13 (case-lambda/? (() 'a) (((x string?) (y char?)) (values x y))))
(check-AV/msg/AN "argument check failed" y (f13 "" 'oops))
(define/? f14 (case-lambda/? (#(rest char?) (reverse rest)) (() 'b)))
(check-AV/msg/AN "argument check failed" rest (f14 1 2 3))
(define/? f15 (case-lambda/? 
                ((x (y string?) z . #(r (lambda (r) (for-all negative? r)))) 
                 (cons* x y z r)) 
                ((x) 'b)))
(check-AV/msg/AN "argument check failed" r (f15 'x "yy" #\z 1 -56.3 67/902))

(define/? f16 (lambda/? () 'a))
(check (f16) => 'a)
(define/? f17 (lambda/? (x) 'b))
(check (f17 1) => 'b)
(define/? f18 (lambda/? ((x integer?)) 'b))
(check (f18 1) => 'b)
(define/? f19 (lambda/? rest (reverse rest)))
(check (f19 1 2 3) => '(3 2 1))
(define/? f20 (lambda/? #(rest list?) (reverse rest)))
(check (f20 1 2 3) => '(3 2 1))
(define/? f21 (lambda/? (x (y string?) z . #(r (lambda (r) (for-all number? r)))) 
                (cons* x y z r)))
(check (f21 'x "yy" #\z 1 -56.3 67/902) => '(x "yy" #\z 1 -56.3 67/902))
(define/? f22 (lambda/? ((x symbol?) (y string?) (z char?)
                         . #(r (lambda (r) (for-all number? r)))) 
                (cons* x y z r)))
(check (f22 'x "yy" #\z 1 -56.3 67/902) => '(x "yy" #\z 1 -56.3 67/902))
(check-syntax-error (define/? f (lambda/? ((x)) 'b)))
(check-syntax-error (define/? f (lambda/? ((x integer? oops)) 'b)))
(check-syntax-error (define/? f (lambda/? #() 'a)))
(check-syntax-error (define/? f (lambda/? #((rest oops)) (reverse rest))))
(check-syntax-error (define/? f (lambda/? #(rest list? oops) (reverse rest))))
(check-syntax-error
 (define/? f (lambda/? (x (y string?) z . #((r (lambda (r) (for-all number? r))))) 
               (cons* x y z r))))
(define/? f23 (lambda/? ((x string?)) 'b))
(check-AV/msg/AN "argument check failed" x (f23 1))
(define/? f24 (lambda/? ((x string?) (y char?)) (values x y)))
(check-AV/msg/AN "argument check failed" y (f24 "" 'oops))
(define/? f25 (lambda/? #(rest char?) (reverse rest)))
(check-AV/msg/AN "argument check failed" rest (f25 1 2 3))
(define/? f26 (lambda/? (x (y string?) z . #(r (lambda (r) (for-all negative? r)))) 
                (cons* x y z r)))
(check-AV/msg/AN "argument check failed" r (f26 'x "yy" #\z 1 -56.3 67/902))

(check (let ()
         (define/? (f) 'a)
         (f))
       => 'a)
(check (let ()
         (define/? (f x) 'b) 
         (f 1))
       => 'b)
(check (let ()
         (define/? (f (x integer?)) 'b)
         (f 1))
       => 'b)
(check (let ()
         (define/? (f . rest) (reverse rest))
         (f 1 2 3))
       => '(3 2 1))
(check (let ()
         (define/? (f . #(rest list?)) (reverse rest))
         (f 1 2 3))
       => '(3 2 1))
(check (let ()
         (define/? (f x (y string?) z . #(r (lambda (r) (for-all number? r)))) 
           (cons* x y z r))
         (f 'x "yy" #\z 1 -56.3 67/902))
       => '(x "yy" #\z 1 -56.3 67/902))
(let ((a 0) (b 0) (c 0) (d 0))
  (define/? (f (x (begin (set! a (+ 1 a)) symbol?))
               (y (begin (set! b (+ 1 b)) string?))
               (z (begin (set! c (+ 1 c)) char?)) 
               . #(r (begin (set! d (+ 1 d))
                            (lambda (r) (for-all number? r))))) 
    (cons* x y z r))
  (check (f 'x "yy" #\z 1 -56.3 67/902)
         => '(x "yy" #\z 1 -56.3 67/902))
  (check (f 'foo "bar" #\c 3 2 1)
         => '(foo "bar" #\c 3 2 1))
  (check (list a b c d) => '(2 2 2 2)))
(let ()
  (define/? (f (x (if (negative? y) string? list?)) y)
    'ok)
  (check (f '(foo) 1) => 'ok)
  (check (f "foo" -1) => 'ok)
  (check-AV/msg/AN "argument check failed" x
    (f "foo" 1))
  (check-AV/msg/AN "argument check failed" x
    (f '(foo) -1)))
(check-AV/who foo
  (let ()
    (define/? foo (lambda/? ((x char?)) x))
    (foo 1)))
(check-AV/who bar
  (let ()
    (define/? bar (case-lambda/? (((x char?)) x)))
    (bar 1)))
(check-syntax-error
 (let ()
   (define/? (f (x)) 'b) 
   (f 1)))
(check-syntax-error
 (let ()
   (define/? (f (x integer? oops)) 'b)
   (f 1)))
(check-syntax-error
 (let ()
   (define/? (f #()) 'a)
   (f)))
(check-syntax-error
 (let ()
   (define/? (f . #((rest oops))) (reverse rest))
   (f 1 2 3)))
(check-syntax-error
 (let ()
   (define/? (f . #(rest list? oops)) (reverse rest))
   (f 1 2 3)))
(check-syntax-error
 (let ()
   (define/? (f x (y string?) z . #((r (lambda (r) (for-all number? r))))) 
     (cons* x y z r))
   (f 'x "yy" #\z 1 -56.3 67/902)))
(check-AV/msg/AN "argument check failed" x
 (let ()
   (define/? (f (x string?)) 'b)
   (f 1)))
(check-AV/msg/AN "argument check failed" y
 (let ()
   (define/? (f (x string?) (y char?)) (values x y))
   (f "" 'oops)))
(check-AV/msg/AN "argument check failed" rest
 (let ()
   (define/? (f . #(rest char?)) (reverse rest))
   (f 1 2 3)))
(check-AV/msg/AN "argument check failed" r
 (let ()
   (define/? (f x (y string?) z . #(r (lambda (r) (for-all negative? r)))) 
     (cons* x y z r))
   (f 'x "yy" #\z 1 -56.3 67/902)))

;;;; define/?/AV

(define/?/AV f27 (case-lambda/? (() 'a) ((x) 'b)))
(check (f27) => 'a)
(define/?/AV f28 
  (case-lambda/?
    (((x symbol?) (y string?) (z char?) . #(r (lambda (r) (for-all number? r)))) 
     (cons* x y z r)) 
    ((x) 'b)))
(check (f28 'x "yy" #\z 1 -56.3 67/902) => '(x "yy" #\z 1 -56.3 67/902))
(define/?/AV f29 (case-lambda/? (() 'a) (r 'b)))
(check (f29 1) => 'b)
(check-syntax-error
 (define/?/AV f 
   (case-lambda/? 
     ((x (y string?) z . #((r (lambda (r) (for-all number? r))))) 
      (cons* x y z r)) 
     ((x) 'b))))
(define/?/AV f30 
  (case-lambda/?
    ((x (y string?) z . #(r (lambda (r) (for-all negative? r)))) 
     (cons* x y z r)) 
    ((x) 'b)))
(check-AV/msg/AN "argument check failed" r (f30 'x "yy" #\z 1 -56.3 67/902))
(define/?/AV f31 
  (case-lambda/? (() (AV "oops")) (#(r (lambda (x) (for-all integer? x))) r)))
(check (f31 1 2 3) => '(1 2 3))
(define/?/AV f32 (case-lambda/? (((s symbol?)) (AV "oops1" 'ign)) (ign #F)))
(check-AV/msg "oops1" (f32 'blah))

(define/?/AV f33 (lambda/? () 'a))
(check (f33) => 'a)
(define/?/AV f34 
  (lambda/? ((x symbol?) (y string?) (z char?) . #(r (lambda (r) (for-all number? r)))) 
    (cons* x y z r)))
(check (f34 'x "yy" #\z 1 -56.3 67/902) => '(x "yy" #\z 1 -56.3 67/902))
(define/?/AV f35 (lambda/? r r))
(check (f35 1) => '(1))
(check-syntax-error
 (define f (lambda/? (x (y string?) z . #((r (lambda (r) (for-all number? r))))) 
             (cons* x y z r))))
(define/?/AV f36 (lambda/? (x (y string?) z . #(r (lambda (r) (for-all negative? r)))) 
                   (cons* x y z r)))
(check-AV/msg/AN "argument check failed" r (f36 'x "yy" #\z 1 -56.3 67/902))
(define/?/AV f37 (lambda/? ((a string?)) (list->string (reverse (string->list a)))))
(check (f37 "asdf") => "fdsa")
(define/?/AV f38 (lambda/? #(r null?) (AV "oops2")))
(check-AV/msg "oops2" (f38))

(check (let ()
         (define/?/AV (f) 'a)
         (f))
       => 'a)
(check (let ()
         (define/?/AV (f (x symbol?) (y string?) (z char?) 
                         . #(r (lambda (r) (for-all number? r)))) 
           (cons* x y z r))
         (f 'x "yy" #\z 1 -56.3 67/902))
       => '(x "yy" #\z 1 -56.3 67/902))
(check
 (let ()
   (define/?/AV (f . r) 'b)
   (f 1))
 => 'b)
(let ()
  (define/?/AV asdf (lambda/? ((x char?)) (AV "oops")))
  (check-AV/who asdf
    (asdf 1))
  (check-AV/who asdf
    (asdf #\c)))
(let ()
  (define/?/AV asdf (case-lambda/? (((x char?)) x) (() (AV "oops"))))
  (check-AV/who asdf
    (asdf 1))
  (check-AV/who asdf
    (asdf)))
(check-syntax-error
 (let ()
   (define/?/AV (f x (y string?) z . #((r (lambda (r) (for-all number? r))))) 
     (cons* x y z r))
   (f 'x "yy" #\z 1 -56.3 67/902)))
(check-AV/msg/AN "argument check failed" r
 (let ()
   (define/?/AV (f x (y string?) z . #(r (lambda (r) (for-all negative? r)))) 
     (cons* x y z r))
   (f 'x "yy" #\z 1 -56.3 67/902)))
(let ()
  (define/?/AV (f x (y integer?) . zs) 
    (when (null? zs) (AV "zs null" zs))
    (apply * x y zs))
  (check (f 1 2 3 4 5) => 120))
(check-AV/msg "oops3"
 (let ()
   (define/?/AV (f) (AV "oops3" 'ign 'ign 'ign))
   (f)))


(check-report)
