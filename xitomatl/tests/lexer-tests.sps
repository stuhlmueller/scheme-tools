#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (except (rnrs) number?)
  (srfi :78 lightweight-testing)
  (only (xitomatl exceptions) catch)
  (only (xitomatl predicates) not? or?)
  (only (xitomatl enumerators) fold/enumerator)
  (only (xitomatl define) define-values)
  (only (xitomatl irregex counting) counted-chunk-underlying)
  (xitomatl lexer))

(define-syntax check-AV
  (syntax-rules ()
    ((_ expr)
     (check (catch ex ((else (assertion-violation? ex)))
              expr
              'unexpected-return)
            => #T))))

(define-syntax check-lexer-ex
  (syntax-rules ()
    ((_ obj idx expr)
     (check (catch ex ((else (and (lexer-condition? ex)
                                  (eq? (lexer-condition-object ex) obj)
                                  (= (lexer-condition-index ex) idx))))
              expr
              'unexpected-return)
            => #T))))


(define (tokr0 str i)
  (define char-other? (not? (or? char-whitespace? char-alphabetic? char-numeric?)))
  (define str-len (string-length str))
  (define (get pred)
    (define (done a) (list->string (reverse a)))
    (let loop ((i i) (a '()))
      (if (< i str-len)
        (let ((c (string-ref str i)))
          (if (pred c)
            (loop (+ 1 i) (cons c a))
            (done a)))
        (done a))))
  (if (< i str-len)
    (let* ((c (string-ref str i))
           (token
            (cond ((char-whitespace? c) `(space ,(get char-whitespace?)))
                  ((char-alphabetic? c) `(word ,(get char-alphabetic?)))
                  ((char-numeric? c) `(number ,(get char-numeric?)))
                  (else `(other ,(get char-other?)))))
           (lexeme (cadr token))
           (end (+ i (string-length lexeme))))
      (if (< end str-len)
        (values token str end)
        (values token #F #F)))
    (values #F #F #F)))

(define (tokr1 str i)
  (let-values (((token ns ni) (tokr0 str i)))
    (case (and (list? token) (car token))
      ((number) (values #F #F #F))
      (else (values token ns ni)))))

(define text0 "This  is a   sentence \n with:   1)  uneven spacing, and 2) blah...")

;;;; lexer -- uses lexer-enumerator

(check ((lexer tokr0) text0)
       => '((word "This") (space "  ") (word "is") (space " ") (word "a") (space "   ")
            (word "sentence") (space " \n ") (word "with") (other ":") (space "   ")
            (number "1") (other ")") (space "  ") (word "uneven") (space " ")
            (word "spacing") (other ",") (space " ") (word "and") (space " ")
            (number "2") (other ")") (space " ") (word "blah") (other "...")))
(let ((empty-str ""))
  (check-lexer-ex empty-str 0
    ((lexer tokr0) empty-str)))
(check-lexer-ex text0 32
  ((lexer tokr1) text0))

;;;; lexer-enumerator -- the aspects not tested above

(check (fold/enumerator (lexer-enumerator tokr0) text0
        (lambda (token n)
          (if (string=? "uneven" (cadr token))
            (values #F n)
            (values #T (+ 1 n))))
        0)
       => 14)
(check (let ((count 0))
         (catch ex (((lexer-condition? ex) count))
           (fold/enumerator (lexer-enumerator tokr1) text0
            (lambda (token)
              (set! count (+ 1 count))
              #T))
           'unexpected-return))
       => 11)

;;;; combine-tokenizers/precedence and combine-tokenizers/exclusive

(define (str-tokr str)
  (define str-len (string-length str))
  (lambda (text i)
    (define text-len (string-length text))
    (define end (+ i str-len))
    (if (<= end text-len)
      (let ((sub (substring text i end)))
        (if (string=? sub str)
          (if (< end text-len)
            (values sub text end)
            (values sub #F #F))
          (values #F #F #F)))
      (values #F #F #F))))

(define text1 "foobarzabbo")
(define-values (tokr2 tokr3)
  (let ((tokrs (map str-tokr '("zabbo" "foo" "zab" "bar" "foobar" "bo"))))
    (values (apply combine-tokenizers/precedence tokrs)
            (apply combine-tokenizers/exclusive tokrs))))

(check-lexer-ex text1 0
  ((lexer (combine-tokenizers/precedence)) text1))
(let ((text "bobonopefoo"))
  (check-lexer-ex text 4
    ((lexer tokr2) text)))
(check ((lexer tokr2) text1)
       => '("foo" "bar" "zabbo"))

(check-lexer-ex text1 0
  ((lexer (combine-tokenizers/exclusive)) text1))
(check-lexer-ex text1 0
  ((lexer tokr3) text1))
(let ((text "barfoozabbo"))
  (check-lexer-ex text 6
    ((lexer tokr3) text)))
(let ((text "bobonopefoo"))
  (check-lexer-ex text 4
    ((lexer tokr3) text)))
(let ((text "bobarzabfoozab"))
  (check ((lexer tokr3) text)
         => '("bo" "bar" "zab" "foo" "zab")))

;;;; define-simple-lexer -- uses simple-lexer/who and sre-tokenizer

(define-simple-lexer slexr0
  (never ,(lambda (c-chunker)
            (lambda (c-chunk idx)
              (set! never-count (+ 1 never-count))
              (values #F #F #F))))
  (word '(+ alphabetic))
  (number '(+ numeric))
  (space '(+ whitespace))
  (other '(+ (~ (or alphabetic numeric whitespace))))
  (shadowed '(+ any)))

(define never-count 0)

(define (check-tokens val specs)
  (and (list? val)
       (= (length val) (length specs))
       (for-all counted-token? val)
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

(let ((specs
       `((,word? "This" 0 0 0) (,space? "  " 4 0 4) (,word? "is" 6 0 6)
         (,space? " " 8 0 8) (,word? "a" 9 0 9) (,space? "   " 10 0 10)
         (,word? "sentence" 13 0 13) (,space? " \n " 21 0 21) (,word? "with" 24 1 1)
         (,other? ":" 28 1 5) (,space? "   " 29 1 6) (,number? "1" 32 1 9)
         (,other? ")" 33 1 10) (,space? "  " 34 1 11) (,word? "uneven" 36 1 13)
         (,space? " " 42 1 19) (,word? "spacing" 43 1 20) (,other? "," 50 1 27)
         (,space? " " 51 1 28) (,word? "and" 52 1 29) (,space? " " 55 1 32)
         (,number? "2" 56 1 33) (,other? ")" 57 1 34) (,space? " " 58 1 35)
         (,word? "blah" 59 1 36) (,other? "..." 63 1 40))))
  (check (slexr0 text0) (=> check-tokens) specs)
  (check (slexr0 (open-string-input-port text0)) (=> check-tokens) specs))
(check-AV (slexr0 'oops))
(let ((empty-str ""))
  (check (catch ex (((lexer-condition? ex)
                     (and (eq? (car (counted-chunk-underlying
                                     (lexer-condition-object ex)))
                               empty-str)
                          (= (lexer-condition-index ex) 0))))
           (slexr0 empty-str)
           'unexpected-return)
         => #T))
(check never-count => (+ 26 26 1))


(check-report)
