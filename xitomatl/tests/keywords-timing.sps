#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import 
  (rnrs)
  (only (xitomatl common) add1 time)
  (xitomatl keywords))

(define-syntax bigloop
  (syntax-rules ()
    ((_ n body ...)
     (let loop ((i 0))
       (unless (= i n)
         body ...
         (loop (add1 i)))))))

(define N #e1e8)
(display "\nN = ") (display N) (newline)

(define (n1 a) 1)
(define (n2 a b) 1)
(define (n3 a b c) 1)

(define/kw (dekw1 (a)) 1)
(display "\nOne argument, define/kw expand-time processing:\n")
(display "===============================================\n")
(time (bigloop N))
(time (bigloop N (n1 1)))
(time (bigloop N (dekw1 'a 1)))

(define/kw (dekw2 (a) (b)) 1)
(display "\nTwo arguments, define/kw expand-time processing:\n")
(display "================================================\n")
(time (bigloop N))
(time (bigloop N (n2 1 2)))
(time (bigloop N (dekw2 'b 2 'a 1)))

(define/kw (dekw3 (a) (b) (c)) 1)
(display "\nThree arguments, define/kw expand-time processing:\n")
(display "==================================================\n")
(time (bigloop N))
(time (bigloop N (n3 1 2 3)))
(time (bigloop N (dekw3 'b 2 'c 3 'a 1)))

(define lekw1 (lambda/kw ((a)) 1))
(display "\nOne argument, lambda/kw run-time processing:\n")
(display "===============================================\n")
(time (bigloop N))
(time (bigloop N (n1 1)))
(time (bigloop N (lekw1 'a 1)))

(define lekw2 (lambda/kw ((a) (b)) 1))
(display "\nTwo arguments, lambda/kw run-time processing:\n")
(display "===============================================\n")
(time (bigloop N))
(time (bigloop N (n2 1 2)))
(time (bigloop N (lekw2 'b 2 'a 1)))

(define lekw3 (lambda/kw ((a) (b) (c)) 1))
(display "\nThree arguments, lambda/kw run-time processing:\n")
(display "===============================================\n")
(time (bigloop N))
(time (bigloop N (n3 1 2 3)))
(time (bigloop N (lekw3 'b 2 'c 3 'a 1)))
