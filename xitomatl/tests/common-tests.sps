#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (srfi :78 lightweight-testing)
  (only (xitomatl exceptions) catch)
  (xitomatl common))

(define-syntax check-AV
  (syntax-rules ()
    ((_ expr)
     (check (catch ex ((else (assertion-violation? ex)))
              expr
              'unexpected-return)
            => #T))))

;; add1
(check (add1 1) => 2)
(check (add1 -123) => -122)
(check (add1 (greatest-fixnum)) => (+ 1 (greatest-fixnum)))
(check-AV (add1 "oops"))
;; sub1
(check (sub1 1) => 0)
(check (sub1 -123) => -124)
(check (sub1 (least-fixnum)) => (- (least-fixnum) 1))
(check-AV (sub1 "oops"))
;; format   TODO?: more comprehensive
(check (format "~~" ) => "~")
(check (format "~s" '(a "b" #\c)) => "(a \"b\" #\\c)")
(check (format "~a" '(a "b" #\c)) => "(a b c)")
(check (format "~b" #b10101100) => "10101100")
(check (format "~o" #o7654321) => "7654321")
(check (string-upcase (format "~x" #xB)) => "B")
(check (string-upcase (format "~x" #xDEADBEEF)) => "DEADBEEF")
;; printf
(let ((fn "/tmp/xitomatl-common-tests--printf"))
  (with-output-to-file fn
    (lambda () (printf "\nTHIS ~s ~x ~a ~~ ~b ~o\n" "is" 10 'TEST 255 8)))
  (check (string-upcase (call-with-input-file fn get-string-all)) 
         => "\nTHIS \"IS\" A TEST ~ 11111111 10\n")
  (delete-file fn))
;; fprintf
(let-values (((sop get) (open-string-output-port)))
  (fprintf sop "\nTHIS ~s ~x ~a ~~ ~b ~o\n" "is" 10 'TEST 255 8)
  (check (string-upcase (get)) => "\nTHIS \"IS\" A TEST ~ 11111111 10\n"))
;; pretty-print
(let ((fn "/tmp/xitomatl-common-tests--pretty-print"))
  (with-output-to-file fn
    (lambda () (pretty-print '(a "b" #\c))))
  (check (call-with-input-file fn read) => '(a "b" #\c))
  (check (let ((s (call-with-input-file fn get-string-all)))
           (string-ref s (- (string-length s) 1)))
         => #\newline)
  (delete-file fn))
(let-values (((sop get) (open-string-output-port)))
  (pretty-print '(a "b" #\c) sop)
  (let ((s (get)))
    (check (read (open-string-input-port s)) => '(a "b" #\c))
    (check (string-ref s (- (string-length s) 1)) => #\newline)))
;; gensym
(let ((g (gensym)))
  (check (symbol? g) => #T)
  (check (eq? g g) => #T)
  (check (symbol=? g g) => #T)
  (check (eq? g (string->symbol (symbol->string g))) => #F)
  (check (symbol=? g (string->symbol (symbol->string g))) => #F))
(check (symbol? (gensym 'foo)) => #T)
(check (symbol? (gensym "foo")) => #T)
;; time -- how to test...?
#;(check  => )
;; with-input-from-string
(check (with-input-from-string "(1 \"two\" #\\3 #(4/5) . six)" read)
       => '(1 "two" #\3 #(4/5) . six))
;; with-output-to-string
(check (with-output-to-string 
        (lambda () (display "something")))
       => "something")


(check-report)
