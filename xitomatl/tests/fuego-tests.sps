#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import 
  (rnrs)
  (xitomatl fuego)
  (srfi :78 lightweight-testing))

(define-syntax check-exn
  (syntax-rules ()
    ((_ expr => pred)
     (check (guard (ex ((pred ex) #T) (else #F))
              expr
              'returned)
            => #T))))

(define (fuego-exn? ex msg irrts obj)
  (and (assertion-violation? ex)
       (who-condition? ex)
       (equal? "(library (xitomatl fuego))" (condition-who ex))
       (message-condition? ex)
       (equal? msg (condition-message ex))
       (irritants-condition? ex)
       (equal? (condition-irritants ex) irrts)
       (fuego-condition? ex)
       (eq? (condition-fuego-object ex) obj)))

(define (unknown-exn? irrts obj)
  (lambda (ex) (fuego-exn? ex "unknown key" irrts obj)))

(define (already-exists-exn? irrts obj)
  (lambda (ex) (fuego-exn? ex "slot already exists" irrts obj)))

(define (immutable-exn? irrts obj)
  (lambda (ex) (fuego-exn? ex "immutable value" irrts obj)))

(define (cycle-exn? irrts obj)
  (lambda (ex) (fuego-exn? ex "parent cycle" irrts obj)))

(define root-keys
  (list :clone :unknown :already-exists :has? :keys
        :add-method! :add-parent! :add-value! :delete!))

;;;; root-object

(check (send root-object :keys) 
       (=> (lambda (r e) (for-all (lambda (x) (memq x r)) e))) 
       root-keys)
(check (for-all (lambda (rk) (send root-object :has? rk))
                root-keys)
       => #T)
(check-exn (send root-object 'oops) 
           => (unknown-exn? '(oops) root-object))
(check-exn (send root-object :add-value! :clone 1) 
           => (already-exists-exn? (list :clone) root-object))


(define (basic-tests o)
  (check-exn (send o 'oops) 
             => (unknown-exn? '(oops) o))
  (send o :add-method! 'm (lambda (s r . args) (cons s args)))
  (send o :add-method! 'm2 (lambda args args))
  (check (send o 'm 1 "two") => (list o 1 "two"))
  (check-exn (send o :add-value! 'm 1) 
             => (already-exists-exn? '(m) o))
  (send o :add-value! 'vi 1)
  (check (send o 'vi) => 1)
  (check-exn (send o 'vi "asdf")
             => (immutable-exn? '(vi "asdf") o))
  (send o :add-value! 'vm 2 #T)
  (check (send o 'vm) => 2)
  (send o 'vm "asdf")
  (check (send o 'vm) => "asdf")
  (check (send o :keys) 
         (=> (lambda (r e) (for-all (lambda (x) (memq x r)) e))) 
         '(m m2 vi vm))
  (check (for-all (lambda (k) (send o :has? k))
                  '(m vi vm))
         => #T)
  (send o :delete! 'm2)
  (check (send o :keys) 
         (=> (lambda (r e) (for-all (lambda (x) (memq x r)) e))) 
         '(m vi vm))
  (check (send o :has? 'm2) => #F)
  (check-exn (send o 'm2 'x "y") 
             => (unknown-exn? '(m2) o))
  (send o :delete! 'vi)
  (check (send o :keys) 
         (=> (lambda (r e) (for-all (lambda (x) (memq x r)) e))) 
         '(m vm))
  (check (send o :has? 'vi) => #F)
  (check-exn (send o 'vi) 
             => (unknown-exn? '(vi) o)))


;;;; clone of root-object

(define o0 (send root-object :clone))
(check (fuego-object? o0) => #T)
(check (length (send o0 :keys)) => 1)
(check (eq? root-object (send o0 (car (send o0 :keys)))) 
       => #T)
(basic-tests o0)

;;;; inheritance / slot resolution

(define o1 (send o0 :clone))
;; same tests as o0
(check (fuego-object? o1) => #T)
(check (length (send o1 :keys)) => 1)
(check (eq? o0 (send o1 (car (send o1 :keys)))) 
       => #T)
(basic-tests o1)
;; o1 overriding o0
(check (send o1 'vm) => "asdf")
(check (send o1 'm 'a 2 "x") => (list o1 'a 2 "x"))
;; o1 using resend
(send o1 :delete! 'm)
(send o1 :add-method! 'm (lambda (s resend . a) (resend 'm a)))
(check (send o1 'm 'foo "bar") => (list o1 '(foo "bar")))
;; resend lookup through more than one parent
(let ((ob (object (parent (object (parent (object))))
                  (parent (object (parent (object))
                                  (parent (object (parent 
                                                    (object (parent (object (parent (object))))))
                                                  (parent o1)))))
                  (method ('m s r . a) (r 'm a)))))
  (check (send ob 'm 'bar "foo")
         => (list ob '((bar "foo")))))
;; resend delivers :unknown to the owner of the method which uses it
(check-exn (send o1 'r-oops) => (unknown-exn? '(r-oops) o1))
(send o0 :add-method! 'r-oops (lambda (s r) (r 'asdf)))
(check-exn (send o1 'r-oops)
           => (unknown-exn? '(asdf) o0)) ;; note that it's o0 which gets the :unknown
;; o1 using parent's slots
(send o1 :delete! 'vm)
(check (send o1 :has? 'vm) => #F)
(check (send o1 'vm) => "asdf")
(send o1 :delete! 'm)
(check (send o1 :has? 'm) => #F)
(check (send o1 'm 'a 2 "x") => (list o1 'a 2 "x"))
;; Setting parent's value causes new slot to be allocated in the immediate
;; instance for the new value so that the parent's stays unchanged.
(send o1 'vm #\λ)
(check (send o1 'vm) => #\λ)
(check (send o1 :has? 'vm) => #T)
(check (send o1 :keys) 
       (=> (lambda (r e) (for-all (lambda (x) (memq x r)) e))) 
         '(vm))
(check (send o0 'vm) => "asdf")
;; add new slot to o0, use from o1
(send o0 :add-value! 'XYZ "acme")
(check (send o1 'XYZ) => "acme")
;; can not create inheritance cycle (prevents infinite loop when searching parents)
(check-exn (send o0 :add-parent! 'P o1)
           => (cycle-exn? (list o1) o0))
(check-exn (send o0 :add-parent! 'P o0)  ;; Note attempt to add self as parent
           => (cycle-exn? (list o0) o0))
;; override unknown key and already-exists
(send o1 :add-method! :unknown (lambda (s r k . vs) (send s :add-value! k vs)))
(send o1 'oops 47 "blah")
(check (send o1 'oops) => '(47 "blah"))
(send o1 :add-method! :already-exists (lambda (s r . args) (reverse (cons s args))))
(check (send o1 :add-method! :unknown +) => (list + :unknown o1))

;; multiple parents
(define o2 (send root-object :clone))
(send o2 :add-method! 'm (lambda (s r . args) (length args)))
(send o1 :add-parent! 'p o2)
(send o1 :add-method! 'm (lambda (s r . args) (cons s (reverse args))))
(check (send o1 'm 1 2 3) => (list o1 3 2 1)) ;; o1's new 'm used
(send o1 :delete! 'm)
(check (send o1 'm 1 2 3) => (list o1 1 2 3)) ;; first parent o0's 'm used
(send o0 :delete! 'm)
(check (send o1 'm 1 2 3) => 3) ;; second parent o2's 'm used

;;;; unusual, intentionally allowed, reconfigurations of lower levels, "meta
;;;; class" abilities
;; NOT YET SURE

;; Object syntax, when used in a recursive region like define or letrec, lambdas
;; can refer to the object being constructed, and full Scheme <body> form
;; evaluation for doing rarer manual configuring of the new object, internally
;; defined helpers, etc.

(define o3 (object))
(check (fuego-object? o3) => #T)
(check (length (send o3 :keys)) => 1)
(check (eq? root-object (send o3 (car (send o3 :keys)))) 
       => #T)
(basic-tests o3)
(define Pk (make-key 'Pk))
(define Mk (list 'Mk))
(define Vk (string-copy "Vk"))
;; unquoted field names are the value of any expression,
;; but must be something which can reliably be found via eq? / assq.
(define o4 (object (parent o3)
                   (parent ,Pk (object))
                   (method ('m s r . a) 
                     (vector (null? a)
                             (r 'm a)))
                   (method (,Mk s r) 'hehehe)
                   (method (,:unknown s r k . v) `(WHAT?! ,k . ,v))
                   (value 'v 'VEE)
                   (value ,Vk 'first #T)))
(check (send o4 'vm) => "asdf")  ;; basic-tests added 'vm to o3
(check (send o4 'm) => (vector #T (list o4 '())))
(check (send o4 'v) => 'VEE)
(check (fuego-object? (send o4 Pk)) => #T)
(check (send o4 Mk) => 'hehehe)
(check (send o4 'oops #\a "b" 'c) => '(WHAT?! oops #\a "b" c))
(check (send o4 Vk) => 'first)
(send o4 Vk 'second)
(check (send o4 Vk) => 'second)
;; For each defined fields which uses the syntax of only an identifier,
;; a new key record is created and used as the key, and all such
;; automatically created keys are returned in the return values of the
;; object form following the new object, in the same order they appeared
;; in the object form's body.
(define-values (o5 :plus3 :V :P :self) 
  (object (define (add3 x) (+ 3 x))
          (method (A s r x) (add3 x))
          (value :V 2)
          (parent C o4)
          (method (:self s r) o5)))
(check (send o5 :plus3 4) => 7)
(check (send o5 :V) => 2)
(check (send o5 :P) (=> eq?) o4)
(check (send o5 :self) (=> eq?) o5)
;; Use unquote to refer to keys to override them.
(define o6
  (object (parent o5)
          (method ,:plus3 (case-lambda ((s r x) (r :plus3 x)) 
                                       ((s r x y . a) (map (lambda (n) (r :plus3 n)) 
                                                           (cons* x y a)))))
          (method (,:self s r) o6)))
(check (send o6 :plus3 -3) => 0)
(check (send o6 :plus3 1 2 3 4 5) => '(4 5 6 7 8))
(check (send o6 :self) (=> eq?) o6)


(check-report)
