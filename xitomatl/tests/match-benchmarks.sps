#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (ikarus)
  (prefix (xitomatl match) X:)
  (prefix (xitomatl IU-match) IU:)   ;; Indiana University matcher
  (prefix (xitomatl AS-match) AS:))  ;; Alex Shinn's matcher

(define (C)
  (do ((i 0 (+ 1 i)))
    ((= i 20))
    (collect)))

(define-syntax repeat
  (syntax-rules ()
    ((_ stop expr ...)
     (do ((i 0 (+ 1 i)))
       ((= i stop))
       expr ...))))

(define-syntax run
  (syntax-rules ()
    ((_ expr)
     (begin
       (display "----\n")
       (pretty-print 'expr)
       (display "----\n")
       (guard (ex (else (print-condition ex)))
         (C)
         (time expr))))))

(define-syntax bench
  (syntax-rules ()
    ((_ name X IU AS)
     (begin
       (printf "\n\n=== ~a ===\n\n" 'name)
       (display "xitomatl\n")
       (run X)
       (display "I.U.\n")
       (run IU)
       (display "A.S.\n")
       (run AS)))))

(define hundred-1e6 #e1e8)
(define list-5 (make-list 5))
(define vector-5 (make-vector 5))
(define list-1e7 (make-list #e1e7))
(define vector-1e7 (make-vector #e1e7))

(bench "Loop of most basic"
  (repeat hundred-1e6
    (X:match 1 (1 #T)))
  (repeat hundred-1e6
    (IU:match 1 (1 #T)))
  (repeat hundred-1e6
    (AS:match 1 (1 #T))))

(bench "Loop of alternate clauses"
  (repeat hundred-1e6
    (X:match 1 (() #F) (2 #F) (x #T)))
  (repeat hundred-1e6
    (IU:match 1 (() #F) (2 #F) (,x #T)))
  (repeat hundred-1e6
    (AS:match 1 (() #F) (2 #F) (x #T))))

(bench "Loop of basic list"
  (repeat hundred-1e6
    (X:match list-5 ((a b c d e) #T)))
  (repeat hundred-1e6
    (IU:match list-5 ((,a ,b ,c ,d ,e) #T)))
  (repeat hundred-1e6
    (AS:match list-5 ((a b c d e) #T))))

(bench "Loop of basic vector"
  (repeat hundred-1e6
    (X:match vector-5 (#(a b c d e) #T)))
  (repeat hundred-1e6
    (IU:match vector-5 (#(,a ,b ,c ,d ,e) #T)))
  (repeat hundred-1e6
    (AS:match vector-5 (#(a b c d e) #T))))

(bench "Multiple elements of huge list"
  (X:match list-1e7 ((x ...) #T))
  (IU:match list-1e7 ((,x ...) #T))
  (AS:match list-1e7 ((x ...) #T)))

(bench "Multiple elements of huge vector"
  (X:match vector-1e7 (#(x ...) #T))
  (IU:match vector-1e7 (#(,x ...) #T))
  (AS:match vector-1e7 (#(x ...) #T)))

(bench "Trailing and multiple elements of huge list"
  (X:match list-1e7 ((x ... a b c d e f g h i j k l m n o p) #T))
  (IU:match list-1e7 ((,x ... ,a ,b ,c ,d ,e ,f ,g ,h ,i ,j ,k ,l ,m ,n ,o ,p) #T))
  (AS:match list-1e7 ((x ... a b c d e f g h i j k l m n o p) #T)))

(bench "Leading and trailing and multiple elements of huge vector"
  (X:match vector-1e7 (#(a b c d e f g h x ... i j k l m n o p) #T))
  (IU:match vector-1e7 (#(,a ,b ,c ,d ,e ,f ,g ,h ,x ... ,i ,j ,k ,l ,m ,n ,o ,p) #T))
  (AS:match vector-1e7 (#(a b c d e f g h x ... i j k l m n o p) #T)))
