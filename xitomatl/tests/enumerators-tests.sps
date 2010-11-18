#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (rnrs mutable-pairs)
  (xitomatl enumerators)
  (srfi :78 lightweight-testing)
  (only (xitomatl lists) make-list last-pair)
  (only (xitomatl ports) port-closed?))

(define-syntax check-values
  (syntax-rules ()
    ((_ expr => expected ...)
     (check (let-values ((vals expr)) vals) => (list expected ...)))))

(define-syntax check-AV/msg
  (syntax-rules ()
    ((_ msg expr)
     (check (guard (ex (else (and (assertion-violation? ex)
                                  (message-condition? ex)
                                  (condition-message ex))))
              expr
              'unexpected-return)
            => msg))))

;;----------------------------------------------------------------------------
;; lists
;;----------------------------------------------------------------------------
;; basic
(check-values (fold '() (lambda _ (assert #F)))
              => )
(check-values (fold '() (lambda _ (assert #F)) 1 2 3)
              => 1 2 3)
(check-values (fold '(1) (lambda (x . s) (values #T (cons x s))))
              => '(1))
(check-values (fold '(1 2) (lambda (x . s) (values #T (cons x s))))
              => '(2 (1)))
(check-values (fold '(1 2 3) (lambda (x . s) (values #T (cons x s))))
              => '(3 (2 (1))))
(check-values (fold '(1 2 3 4) (lambda (x . s) (values #T x s)))
              => 4 '(3 (2 (1 ()))))
(check-values (fold '(1 2 3 4 5) (lambda (x . s) (apply values #T x s))) 
              => 5 4 3 2 1)
;; early termination
(check-values (fold '(1 2 3 4 5 6) 
                    (lambda (x s) 
                      (if (< x 4) 
                        (values #T (+ x s))
                        (values #F s)))
                    123) 
              => 129)
(check-values (fold '(1 2 3 4 5 6) 
                    (lambda (x s) 
                      (if (< x 5) 
                        (values #T (+ x s))
                        (values #F s)))
                    123) 
              => 133)
;; varying number of seed values
(check-values (fold '(1 2 3 4 5 6 7) 
                    (lambda (x . s) 
                      (if (odd? x)
                        (apply values #T 'foo s)
                        (apply values #T (make-list x 'y))))
                    'a 'b 'c) 
              => 'foo 'y 'y 'y 'y 'y 'y)
;; exceptions
(check-AV/msg "not a proper list"
  (fold '(1 . oops) (lambda _ #T)))
(check-AV/msg "not a proper list"
  (fold '(1 2 . oops) (lambda _ #T)))
(check-AV/msg "not a proper list"
  (fold '(1 2 3 . oops) (lambda _ #T)))
(check-AV/msg "circular list"
  (fold (let ((t (list 1)))
          (set-cdr! t t)
          t) 
        (lambda _ #T)))
(check-AV/msg "circular list"
  (fold (let* ((t (list 2))
               (l (cons 1 t)))
          (set-cdr! t l)
          l) 
        (lambda _ #T)))
(check-AV/msg "circular list"
  (fold (let* ((t (list 3))
               (l (cons* 1 2 t)))
          (set-cdr! t l)
          l) 
        (lambda _ #T)))
(check-AV/msg "circular list"
  (fold (let ((l (make-list 12345 'foo)))
          (set-cdr! (last-pair l) l)
          l) 
        (lambda _ #T)))

;;----------------------------------------------------------------------------
;; vectors
;;----------------------------------------------------------------------------
;; basic
(check-values (fold '#() (lambda _ (assert #F)))
              => )
(check-values (fold '#() (lambda _ (assert #F)) 1 2 3)
              => 1 2 3)
(check-values (fold '#(1) (lambda (x . s) (values #T (cons x s))))
              => '(1))
(check-values (fold '#(1 2 3 4 5) (lambda (x . s) (apply values #T x s))) 
              => 5 4 3 2 1)
;; early termination
(check-values (fold '#(1 2 3 4 5 6) 
                    (lambda (x s) 
                      (if (< x 4) 
                        (values #T (+ x s))
                        (values #F s)))
                    123) 
              => 129)
;; varying number of seed values
(check-values (fold '#(1 2 3 4 5 6 7) 
                    (lambda (x . s) 
                      (if (odd? x)
                        (apply values #T 'foo s)
                        (apply values #T (make-list x 'y))))
                    'a 'b 'c) 
              => 'foo 'y 'y 'y 'y 'y 'y)

;;----------------------------------------------------------------------------
;; strings (the strings enumerator uses the same implementation as vectors)
;;----------------------------------------------------------------------------
;; basic
(check-values (fold "" (lambda _ (assert #F)))
              => )
(check-values (fold "" (lambda _ (assert #F)) 1 2 3)
              => 1 2 3)
(check-values (fold "1" (lambda (x . s) (values #T (cons x s))))
              => '(#\1))
(check-values (fold "12345" (lambda (x . s) (apply values #T x s))) 
              => #\5 #\4 #\3 #\2 #\1)

;;----------------------------------------------------------------------------
;; procedures
;;----------------------------------------------------------------------------
;; basic
(check-values (fold (lambda () (values))
                    (lambda _ (assert #F)))
              => )
(check-values (fold (lambda () (values))
                    (lambda _ (assert #F))
                    1 2 3)
              => 1 2 3)
(define (make-gen . vals)
  (lambda () 
    (if (null? vals)
      (values)
      (let ((v (car vals)))
        (set! vals (cdr vals))
        v))))
(check-values (fold (make-gen 1 2 3)
                    (lambda (x s)
                      (values #T (+ x s)))
                    123)
              => 129)
;; early termination
(check-values (fold (make-gen 1 2 3 4 5 6)
                    (lambda (x s)
                      (if (< x 5)
                        (values #T (+ x s))
                        (values #F s)))
                    123)
              => 133)
;; varying number of seed values
(check-values (fold (make-gen 1 2 3 4 5 6 7)
                    (lambda (x . s) 
                      (if (odd? x)
                        (apply values #T 'foo s)
                        (apply values #T (make-list x 'y))))
                    'a 'b 'c)
              => 'foo 'y 'y 'y 'y 'y 'y)

;;----------------------------------------------------------------------------
;; custom fold specialization
;;----------------------------------------------------------------------------
(define-record-type thing (fields value))
(fold-specialize!
 thing?
 (lambda (coll proc seeds)
   (apply fold (reverse (thing-value coll)) proc seeds)))
(check-values (fold (make-thing '(1 2 3 4 5)) 
                    (lambda (x . s) (apply values #T x s))) 
              => 1 2 3 4 5)

;;----------------------------------------------------------------------------
;; fold/enumerator and input-port-enumerator
;;----------------------------------------------------------------------------
(define sip0 (open-string-input-port "foo \"bar\" 123"))
(check-values
  (fold/enumerator 
   (input-port-enumerator get-datum)
   sip0 
   (lambda (x s) 
     (values #T (cons x s)))
   '())
  => '(123 "bar" foo))
(check (port-closed? sip0) => #F)
(define sip1 (open-string-input-port "123 foo \"bar\""))
(check-values
  (fold/enumerator 
   (input-port-enumerator get-char)
   sip1 
   (lambda (x s) 
     (values #T (cons x s)))
   '())
  => '(#\" #\r #\a #\b #\" #\space #\o #\o #\f #\space #\3 #\2 #\1))
(check (port-closed? sip1) => #F)
(define text
"Blah 123 ()\n\
\"another\" #\\l #(i n) #\\e\n\
a n d ((()))")
(check (fold/enumerator
        (input-port-enumerator get-datum)
        (open-string-input-port text)
        (lambda (d a)
          (values #T (cons d a)))
        '())
       => '(((())) d n a #\e #(i n) #\l "another" () 123 Blah))
(check (fold/enumerator
        (input-port-enumerator get-char)
        (open-string-input-port text)
        (lambda (c i)
          (if (< i 10)
            (values #T (+ 1 i))
            (values #F c)))
        0)
       => #\))
(check-values (fold/enumerator
               (input-port-enumerator get-char)
               (open-string-input-port text)
               (lambda (c)
                 (values #F 1 2 3 4)))
              => 1 2 3 4)

;;----------------------------------------------------------------------------
;; sequence
;;----------------------------------------------------------------------------
(define u8-e
  (sequence-enumerator bytevector-length bytevector-u8-ref))
;; basic
(check-values (fold/enumerator u8-e #vu8() (lambda _ (assert #F)))
              => )
(check-values (fold/enumerator u8-e #vu8() (lambda _ (assert #F)) 1 2 3)
              => 1 2 3)
(check-values (fold/enumerator u8-e #vu8(1) (lambda (x . s) (values #T (cons x s))))
              => '(1))
(check-values (fold/enumerator u8-e #vu8(1 2 3 4 5) 
                               (lambda (x . s) (apply values #T x s))) 
              => 5 4 3 2 1)

;;----------------------------------------------------------------------------
;; hashtables
;;----------------------------------------------------------------------------
(define (plist->hashtable . plist)
  (let ((ht (make-eq-hashtable)))
    (let loop ((plist plist))
      (unless (null? plist)
        (hashtable-set! ht (car plist) (cadr plist))
        (loop (cddr plist))))
    ht))
;; basic
(check-values (fold (plist->hashtable) (lambda _ (assert #F)))
              => )
(check-values (fold (plist->hashtable) (lambda _ (assert #F)) 1 2 3)
              => 1 2 3)
(check-values (fold (plist->hashtable 'a 1) (lambda (x . s) (values #T (cons x s))))
              => '((a . 1)))
(check (let-values ((r (fold (plist->hashtable 'a 1 'b 2 'c 3 'd 4 'e 5) 
                             (lambda (x . s) (apply values #T x s)))))
         (for-all (lambda (x) (and (member x r) #T))
                  '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5)))) 
       => #T)
;; early termination
(check-values (fold (plist->hashtable 'a 1 'b 2 'c 3 'd 4 'e 5 'f 6) 
                    (lambda (x s) (values #F (+ 1 s)))
                    123) 
              => 124)
;; varying number of seed values
(check-values (fold (plist->hashtable 'a 1 'b 2 'c 3 'd 4 'e 5 'f 6 'g 7) 
                    (lambda (x . s) 
                      (if (odd? (cdr x))
                        (apply values #T 'foo s)
                        (apply values #T s)))
                    'a 'b 'c) 
              => 'foo 'foo 'foo 'foo 'a 'b 'c)



(check-report)
