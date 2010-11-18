#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (except (rnrs) file-exists? delete-file)
  (srfi :78 lightweight-testing)
  (xitomatl datum-find)
  (xitomatl enumerators)
  (xitomatl match)
  (xitomatl file-system base)
  (only (xitomatl exceptions) reraise))

(define-syntax check-values
  (syntax-rules (=>)
    ((_ expr => vals ...)
     (check (let-values ((v expr)) v) => (list vals ...)))))

;; datum-find-enumerator -- ports

(check (fold/enumerator
        (datum-find-enumerator (lambda _ (assert #F)))
        (open-string-input-port "")
        (lambda _ (assert #F))
        'nothing)
       => 'nothing)
(check-values (fold/enumerator
               (datum-find-enumerator (lambda (_) #T))
               (open-string-input-port "symbol\"string\"#\\c 123")
               (lambda (d a i) 
                 (values #T (cons d a) (+ 1 i)))
               '() 0)
              => '(123 #\c "string" symbol) 4)
(check-values (fold/enumerator
               (datum-find-enumerator char?)
               (open-string-input-port "symbol\"string\"#\\c 123")
               (lambda (d a i) 
                 (values #T (cons d a) (+ 1 i)))
               '() 0)
              => '(#\c) 1)
(check (fold/enumerator
        (datum-find-enumerator (lambda (_) #T))
        (open-string-input-port "symbol\"string\"#\\c 123")
        (lambda (d i) 
          (if (< i 2)
            (values #T (+ 1 i))
            (values #F d)))
        0)
       => #\c)
(check (fold/enumerator
        (datum-find-enumerator (matches? (_ _ ...)))
        (open-string-input-port "(foo () (bar (baz)) 1 zab)")
        (lambda (d a) 
          (values #T (cons d a)))
        '())
       => '((baz) (bar (baz)) (foo () (bar (baz)) 1 zab)))
(check (fold/enumerator
        (datum-find-enumerator (matches? ()))
        (open-string-input-port "(foo () (bar (baz)) 1 zab)")
        (lambda (d a) 
          (values #T (cons d a)))
        '())
       => '(()))
(check (fold/enumerator
        (datum-find-enumerator (matches? (_ (... 1) . (:not (:or (_ . _) ())))))
        (open-string-input-port "(foo (oof . 3) (bar (baz) . 2) 1 zab)")
        (lambda (d a) 
          (values #T (cons d a)))
        '())
       => '((bar (baz) . 2) (oof . 3)))
(check (fold/enumerator
        (datum-find-enumerator (matches? (:regex "fo+(\\w+)" (:or "bar" "zab"))))
        (open-string-input-port 
         "#((\"foooo\") \"foobar\" #(1 (2 . #(#(#(\"fozab\") 3)))))")
        (lambda (d a) 
          (values #T (cons d a)))
        '())
       => '("fozab" "foobar"))
(check (let ((raised 0))
         (check (with-exception-handler
                  (lambda (ex) 
                    (set! raised (+ 1 raised))
                    (reraise ex))
                  (lambda ()
                    (fold/enumerator
                     (datum-find-enumerator (lambda (_) #T) #T)
                     (open-string-input-port "foo bar b\\az 1 2zab)")
                     (lambda (d a) (values #T (cons d a)))
                     '())))
                => '(bar foo))
         raised)
       => 1)

;; datum-find-enumerator -- directory walking

(make-directory "/tmp/xitomatl-tests")
(current-directory "/tmp/xitomatl-tests")
(call-with-output-file "./f0" (lambda (fop) (write '("s" foo #(3 "this" 2)) fop)))
(call-with-output-file "./f1" (lambda (fop) (write "this" fop)))
(make-directory "./d0")
(call-with-output-file "./d0/f2" (lambda (fop) (write '#((#\c "this") #("this")) fop)))
(make-directory "./d0/d1")
(call-with-output-file "./d0/d1/f3" (lambda (fop) (display "ok oo\\ps" fop)))
(make-directory "./d0/d2")
(call-with-output-file "./d0/d2/f4" (lambda (fop) (write "a" fop)))
(change-mode "./d0/d2/f4" #o000) ;; cause open/read error
(call-with-output-file "./d0/d2/f5" (lambda (fop) (write 'b fop)))
(check
 (let ((raised 0))
   (check-values (with-exception-handler
                   (lambda (ex)
                     (when (and (who-condition? ex)
                                (eq? (condition-who ex) 'datum-find-enumerator))
                       (set! raised (+ 1 raised)))
                     (reraise ex))
                   (lambda ()
                     (fold/enumerator
                      (datum-find-enumerator (lambda (_) #T) #T)
                      "./"
                      (lambda (d f all thises)
                        (values #T (list-sort string<? (cons f all)) 
                                (if (equal? d "this") (+ 1 thises) thises)))
                      '() 0)))
                 => '("./d0/d1/f3" "./d0/d2/f5" "./d0/f2" "./d0/f2" "./d0/f2"
                      "./d0/f2" "./d0/f2" "./d0/f2" "./f0" "./f0" "./f0" "./f0"
                      "./f0" "./f0" "./f0" "./f1") 
                    4)
   raised)
 => 2)

;; datum-find 

(let ((count 0))
  ((datum-find number?)
   (lambda (d) (set! count (+ 1 count)))
   (open-string-input-port "1 (#\\2 3) #(\"4\")"))
  (check count => 2))
(let ((found '()))
  ((datum-find symbol?)
   (lambda (d f) (set! found (cons (list d f) found)))
   ".")
  (check (list-sort (lambda (x y) (string<? (cadr x) (cadr y))) found) 
         => '((ok "./d0/d1/f3") (b "./d0/d2/f5") (foo "./f0"))))
(check ((datum-find->list (matches? (:or #vu8(123 3 2 1) 'x)))
        (open-string-input-port "x #(#vu8(2 1) (#vu8(123 3 2 1))) ()"))
       => '(x #vu8(123 3 2 1)))
(check ((datum-find->list (lambda (x) (or (list? x) (vector? x)))) ".")
       => '(("./d0/f2" #((#\c "this") #("this")) (#\c "this") #("this")) 
            ("./f0" ("s" foo #(3 "this" 2)) #(3 "this" 2))))

;; clean-up
(change-mode "./d0/d2/f4" #o600)
(let ((tests-dir (current-directory)))
  (current-directory "/tmp")
  (delete-any tests-dir)
  (check (file-exists? tests-dir) => #F))

(check-report)
