#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (srfi :78 lightweight-testing)
  (only (xitomatl exceptions) catch)
  (only (xitomatl irregex counting) counted-chunk-underlying)
  (xitomatl lexer)
  (xitomatl R6RS-lexer))

(define-syntax check-AV
  (syntax-rules ()
    ((_ who msg expr)
     (check (catch ex (((assertion-violation? ex)
                        (and (who-condition? ex)
                             (eq? 'who (condition-who ex))
                             (message-condition? ex)
                             (string=? msg (condition-message ex)))))
              expr
              'unexpected-return)
            => #T))))

(define (check-tokens val specs)
  (and (list? val)
       (= (length val) (length specs))
       (for-all R6RS-token? val)
       (for-all (lambda (v s)
                  (let ((type? (list-ref s 0))
                        (lexeme (list-ref s 1))
                        (char (list-ref s 2))
                        (line (list-ref s 3))
                        (column (list-ref s 4)))
                    (and (type? v)
                         (string=? (token-lexeme v) lexeme)
                         (= (counted-token-char v) char)
                         (= (counted-token-line v) line)
                         (= (counted-token-column v) column))))
                val specs)))

(define-syntax test
  (syntax-rules (=>)
    ((_ expr => specs)
     (check (R6RS-lexer expr) (=> check-tokens) specs))))

;;;; simple

(check-AV R6RS-lexer "invalid object"
  (R6RS-lexer 'oops))
(test "(Hello\n \"world\" \n   #\\!)"
      => `((,open-paren-token? "(" 0 0 0) (,identifier-token? "Hello" 1 0 1)
           (,whitespace-token? "\n " 6 0 6) (,string-token? "\"world\"" 8 1 1)
           (,whitespace-token? " \n   " 15 1 8) (,character-token? "#\\!" 20 2 3)
           (,close-paren-token? ")" 23 2 6)))

;;;; each lexeme type

(let-syntax ((test (syntax-rules (=>)
                     #;((_ str => #F)
                      )
                     ((_ str => pred?)
                      (test str => `((,pred? str 0 0 0)))))))
  (test "λ" => identifier-token?)
  (test "foo" => identifier-token?)
  ;; TODO: more
  (test ",@" => unquote-splicing-token?)
  (test "#,@" => unsyntax-splicing-token?))

;;;; lexical errors

(let-syntax ((test (syntax-rules ()
                     ((_ str)
                      (let ((s str))
                        (check (catch ex (((lexer-condition? ex)
                                           (and (eq? (car (counted-chunk-underlying
                                                           (lexer-condition-object ex)))
                                                     s)
                                                (= (lexer-condition-index ex) 0))))
                                 (R6RS-lexer s)
                                 'unexpected-return)
                               => #T))))))
  (test "-x")
  (test "5i")
  (test "#\\oops")
  (test "\"cb\\qbc\"")
  (test "#|a#|b|#"))

;;;; full-blown

#;(call-with-input-file ""
  (lambda (fip)
    (test fip
          => `((,?-token? "" ? ? ?)))))


(check-report)
