#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl R6RS-lexer)
  (export
    (rename (R6RS-lexer/seps R6RS-lexer)) R6RS-token?
    identifier-token? make-identifier-token
    boolean-token? make-boolean-token
    number-token? make-number-token
    character-token? make-character-token
    string-token? make-string-token
    open-paren-token? make-open-paren-token
    close-paren-token? make-close-paren-token
    open-bracket-token? make-open-bracket-token
    close-bracket-token? make-close-bracket-token
    open-curly-token? make-open-curly-token
    close-curly-token? make-close-curly-token
    open-vector-token? make-open-vector-token
    open-bytevector-token? make-open-bytevector-token
    quote-token? make-quote-token
    quasiquote-token? make-quasiquote-token
    unquote-splicing-token? make-unquote-splicing-token
    unquote-token? make-unquote-token
    dot-token? make-dot-token
    syntax-token? make-syntax-token
    quasisyntax-token? make-quasisyntax-token
    unsyntax-splicing-token? make-unsyntax-splicing-token
    unsyntax-token? make-unsyntax-token
    whitespace-token? make-whitespace-token
    line-comment-token? make-line-comment-token
    nested-comment-token? make-nested-comment-token
    datum-comment-token? make-datum-comment-token
    hashbang-comment-token? make-hashbang-comment-token
    comment-token? atmosphere-token? abbreviation-token?)
  (import
    (rnrs)
    (srfi :39 parameters)
    (except (xitomatl irregex)
            sre->irregex
            irregex-search/chunked)
    (only (xitomatl regexp)
          compile-SRE
          regexp-search/chunked)
    (only (xitomatl irregex counting)
          line-separators
          counted-match-start-positions)
    #;(xitomatl irregex unicode-general-categories)
    (xitomatl lexer)
    (only (xitomatl predicates)
          or?)
    (for (only (xitomatl macro-utils)
               identifier-append)
         expand))

  (define (sre->irregex sre . _) (compile-SRE sre))
  (define irregex-search/chunked regexp-search/chunked)

  (let-syntax ((def-gen-cats (syntax-rules ()
                               ((_ name ...)
                                (begin (define name 'name) ...)))))
    (def-gen-cats Lu Ll Lt Lm Lo Mn Mc Me Nd Nl No Pc Pd Ps Pe
                  Pi Pf Po Sm Sc Sk So Zs Zl Zp Cc Cf Cs Co Cn))


  (define-record-type R6RS-token (parent counted-token))

  (define-syntax define-R6RS-lexer
    (lambda (stx)
      (syntax-case stx ()
        ((kw (type spec) ...)
         (with-syntax ((R6RS-lexer (datum->syntax #'kw 'R6RS-lexer))
                       ((type ...)
                        (map (lambda (t) (identifier-append t t "-token"))
                             #'(type ...))))
           #'(define-simple-lexer (R6RS-lexer R6RS-token)
               (type spec) ...))))))

  (define whitespace
    `(or #\x9 #\xA #\xB #\xC #\xD #\x85 ,Zs ,Zl ,Zp))
  (define delimiter
    `(or #\( #\) #\[ #\] #\" #\; #\# ,whitespace eos))
  (define inline-hex-escape
    '(: "\\x" (+ hex-digit) #\;))
  (define line-ending
    '(or (: #\xD (? (or #\A #\x85)))
         #\xA #\x85 #\x2028))

  (define-R6RS-lexer
    (identifier
     (let* ((letter
             '(/ #\a #\z #\A #\Z))
            (constituent
             `(or ,letter (- (or ,Lu ,Ll ,Lt ,Lm ,Lo ,Mn ,Nl ,No
                                 ,Pd ,Pc ,Po ,Sc ,Sm ,Sk ,So ,Co)
                             (/ #\x0 #\x7F))))
            (special-initial
             '(or #\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~))
            (initial
             `(or ,constituent ,special-initial ,inline-hex-escape))
            (special-subsequent
             '(or #\+ #\- #\. #\@))
            (subsequent
             `(or ,initial digit ,special-subsequent ,Nd ,Mc ,Me))
            (peculiar-identifier
             `(or #\+ #\- "..." (: "->" (* ,subsequent)))))
       `(: (or (: ,initial (* ,subsequent))
               ,peculiar-identifier)
           (look-ahead ,delimiter))))
    (boolean
     `(: #\# (or #\t #\T #\f #\F)
         (look-ahead ,delimiter)))
    (number
     (let* ((digit-16
             'hex-digit)
            (digit-10
             'digit)
            (digit-8
             '(/ #\0 #\7))
            (digit-2
             '(or #\0 #\1))
            (digit
             (lambda (r)
               (cdr (assv r `((16 . ,digit-16)
                              (10 . ,digit-10)
                              (8 . ,digit-8)
                              (2 . ,digit-2))))))
            (radix-16
             '(: #\# (or #\x #\X)))
            (radix-10
             '(? (: #\# (or #\d #\D))))
            (radix-8
             '(: #\# (or #\o #\O)))
            (radix-2
             '(: #\# (or #\b #\B)))
            (radix
             (lambda (r)
               (cdr (assv r `((16 . ,radix-16)
                              (10 . ,radix-10)
                              (8 . ,radix-8)
                              (2 . ,radix-2))))))
            (exactness
             '(? (: #\# (or #\i #\I #\e #\E))))
            (sign
             '(? (or #\+ #\-)))
            (mantissa-width
             `(? (: #\| (+ ,digit-10))))
            (exponent-marker
             '(or #\e #\E #\s #\S #\f #\F #\d #\D #\l #\L))
            (suffix
             `(? (: ,exponent-marker ,sign (+ ,digit-10))))
            (prefix
             (lambda (r)
               `(or (: ,(radix r) ,exactness)
                    (: ,exactness ,(radix r)))))
            (uinteger
             (lambda (r) `(+ ,(digit r))))
            (decimal-10
             `(or (: ,(uinteger 10) ,suffix)
                  (: #\. (+ ,digit-10) ,suffix)
                  (: (+ ,digit-10) #\. (* ,digit-10) ,suffix)))
            (decimal
             (lambda (r) (and (= 10 r) decimal-10)))  ;; #F gets propagated
            (ureal
             (let ((or (lambda args
                         (and (exists values args)
                              `(or . ,(remq #F args)))))
                   (: (lambda args
                        (and (for-all values args)
                             `(: . ,args)))))
               (lambda (r)
                 (or (uinteger r)
                     `(: ,(uinteger r) #\/ ,(uinteger r))
                     (: (decimal r) mantissa-width)))))
            (naninf
             '(: (or "nan" "inf") ".0"))
            (real
             (lambda (r)
               `(or (: ,sign ,(ureal r))
                    (: (or #\+ #\-) ,naninf))))
            (complex
             (lambda (r)
               (let ((imaginary
                      `(: (or #\+ #\-)
                          (? (or ,(ureal r) ,naninf))
                          #\i)))
                 `(or (: ,(real r)
                         (? (or (: #\@ ,(real r))
                                ,imaginary)))
                      ,imaginary))))
            (num
             (lambda (r) `(: ,(prefix r) ,(complex r)))))
       `(: (or ,(num 10) ,(num 16) ,(num 8) ,(num 2))
           (look-ahead ,delimiter))))
    (character
     `(: "#\\" (or "nul" "alarm" "backspace" "tab"
                   "linefeed" "newline" "vtab" "page"
                   "return" "esc" "space" "delete"
                   (: #\x (+ hex-digit))
                   any)
         (look-ahead ,delimiter)))
    (string
     (let* ((intraline-whitespace `(or #\x9 ,Zs))
            (string-element
             `(or (~ #\" #\\)
                  (: #\\ (or #\a #\b #\t #\n #\v #\f #\r #\" #\\
                             (: (* ,intraline-whitespace)
                                ,line-ending
                                (* ,intraline-whitespace))))
                  ,inline-hex-escape)))
       `(: #\" (* ,string-element) #\")))
    (open-paren
     #\()
    (close-paren
     #\))
    (open-bracket
     #\[)
    (close-bracket
     #\])
    (open-curly
     #\{)
    (close-curly
     #\})
    (open-vector
     "#(")
    (open-bytevector
     "#vu8(")
    (quote
     #\')
    (quasiquote
     #\`)
    (unquote-splicing
     ",@")
    (unquote
     #\,)
    (dot
     #\.)
    (syntax
     "#'")
    (quasisyntax
     "#`")
    (unsyntax-splicing
     "#,@")
    (unsyntax
     "#,")
    (whitespace
     `(+ ,whitespace))
    (line-comment
     `(: #\; (*? any) (or ,line-ending #\x2029)))
    (nested-comment
     ,(let ((initial (sre->irregex '(: bos "#|") 'fast))
            (start/end (sre->irregex '(or "|#" "#|") 'fast)))
        (lambda (c-chunker)
          (define get-substring (chunker-get-substring c-chunker))
          (chunk-tokenizer c-chunker
           (lambda (c-chunk idx)
             (let ((m (irregex-search/chunked initial c-chunker c-chunk idx)))
               (if m
                 (let recur ((c (irregex-match-end-chunk m 0))
                             (i (irregex-match-end-index m 0))
                             (n 1))
                   (if (= 0 n)
                     (values (let-values (((char line column)
                                           (counted-match-start-positions m 0)))
                               (make-nested-comment-token
                                (get-substring (irregex-match-start-chunk m 0)
                                               (irregex-match-start-index m 0)
                                               c i)
                                char line column))
                             c i)
                     (let ((m2 (irregex-search/chunked start/end c-chunker c i)))
                       (if m2
                         (recur (irregex-match-end-chunk m2 0)
                                (irregex-match-end-index m2 0)
                                (let ((str (irregex-match-substring m2)))
                                  (cond ((string=? "|#" str)
                                         (- n 1))
                                        ((string=? "#|" str)
                                         (+ 1 n)))))
                         (values #F #F #F)))))
                 (values #F #F #F))))))))
    (datum-comment
     "#;")   ;; Different than R6RS 4.2.1.
    (hashbang-comment
     "#!"))  ;; Different than R6RS 4.2.1.

  (define (R6RS-lexer/seps x)
    (parameterize ((line-separators
                    '("\xD;\xA;"
                      "\xD;\x85;"
                      "\xD;"
                      "\xA;"
                      "\x85;"
                      "\x2028;")))
      (R6RS-lexer x)))
  
  (define comment-token?
    (or? line-comment-token? nested-comment-token?
         datum-comment-token? hashbang-comment-token?))

  (define atmosphere-token?
    (or? whitespace-token? comment-token?))

  (define abbreviation-token?
    (or? quote-token? quasiquote-token? unquote-token? unquote-splicing-token?
         syntax-token? quasisyntax-token? unsyntax-token? unsyntax-splicing-token?))
)
