#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (xitomatl delimited-control)
  (srfi :78 lightweight-testing))

;------------------------------------------------------------------------
;			Shift tests

(check (+ 10 (reset (+ 2 (shift k (+ 100 (k (k 3))))))) 
       => 117)

(check (* 10 (reset (* 2 (shift g (reset
                                   (* 5 (shift f (+ (f 1) 1))))))))
       => 60)

(check (let ((f (lambda (x) (shift k (k (k x))))))
           (+ 1 (reset (+ 10 (f 100)))))
       => 121)

(check (reset
        (let ((x (shift f (cons 'a (f '())))))
          (shift g x)))
       => '(a))

(define (p x) (if (eq? x p) '(p p) `(p ,x)))
(define (shift* p) (shift f (p f)))
(check (reset (let ((x 'abcde)) (eq? x ((shift* shift*) x))))
       => #T)

(define (traverse/shift xs)
  (letrec ((visit
            (lambda (xs)
              (if (null? xs)
                '()
                (visit (shift k (cons (car xs) (k (cdr xs)))))))))
    (reset (visit xs))))

(check (traverse/shift '(1 2 3 4 5))
       => '(1 2 3 4 5))

;------------------------------------------------------------------------
;			Control tests
; Example from Sitaram, Felleisen

(check (let ((g (prompt (* 2 (control k k)))))
         (* 3 (prompt (* 5 (abort (g 7))))))
       => 42)

(define (traverse/control xs)
  (letrec ((visit
            (lambda (xs)
              (if (null? xs)
                '()
                (visit (control k (cons (car xs) (k (cdr xs)))))))))
    (prompt (visit xs))))

(check (traverse/control '(1 2 3 4 5))
       => '(5 4 3 2 1))

(check (+ 10 (prompt (+ 2 (control k (+ 100 (k (k 3)))))))
       => 117)

(check (prompt (let ((x (control f (cons 'a (f '()))))) (control g x)))
       => '())

(check (prompt ((lambda (x) (control l 2)) (control l (+ 1 (l 0)))))
       => 2)

(check (prompt (control f (cons 'a (f '()))))
       => '(a))

(check (prompt (let ((x (control f (cons 'a (f '()))))) (control g (g x))))
       => '(a))

(check (prompt (let ((x 'abcde)) (eq? x ((control k (control k2 (k k2))) x))))
       => #T)

;------------------------------------------------------------------------
;			control0 and shift0 tests

(check (+ 10 (prompt0 (+ 2 (control0 k (+ 100 (k (k 3)))))))
       => 117)

(check (prompt0 (prompt0
                 (let ((x (control0 f (cons 'a (f '()))))) (control0 g x))))
       => '())

(check (+ 10 (prompt0 (+ 2 (shift0 k (+ 100 (k (k 3)))))))
       => 117)

(check (prompt0 (cons 'a (prompt0 (shift0 f (shift0 g '())))))
       => '())


(check-report)
