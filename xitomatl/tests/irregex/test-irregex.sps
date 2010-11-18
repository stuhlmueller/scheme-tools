#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import 
  (rnrs)
  (only (rnrs r5rs) quotient)
  (xitomatl irregex)
  (xitomatl match)
  (only (xitomatl strings) string-split string-intersperse)
  (only (xitomatl common) format)
  (only (xitomatl ports) port-for-each)
  (xitomatl tests irregex test)
  (xitomatl include))

(define (warning msg . irrts)
  (raise (condition (make-warning) 
                    (make-message-condition msg)
                    (make-irritants-condition irrts))))

(define (call-with-output-string proc)
  (call-with-string-output-port proc))

(define (call-with-input-string str proc)
  (call-with-port (open-string-input-port str) proc))

(define (sprintf fmt-str . args)
  (define (convert x)
    (let loop ((l (string->list x)) (a '()))
      (if (null? l)
        (list->string (reverse a))
        (if (and (pair? (cdr l)) (char=? (car l) #\~))
          (loop (cddr l) (cons* (char-downcase (cadr l)) (car l) a))
          (loop (cdr l) (cons (car l) a))))))
  (apply format (convert fmt-str) args))

(define read-line get-line)

(include/resolve ("xitomatl" "tests" "irregex") "test-irregex.scm")

(test-exit 1800)
