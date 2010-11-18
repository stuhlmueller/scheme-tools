#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Destructuring binding pattern matcher by Derick Eddington
;; 
;; Features:
;; - Regular expression matching against strings, with sub-group matching.
;;   (Utilizes Alex Shinn's IrRegex library).
;; - Record matching, with field matching. 
;; - Arbitrary predicate matching.
;; - "and", "or", and "not" matching.
;; - quasiquote patterns, with the unquote'd expressions evaluated as 
;;   normal expressions in the environment of the match expression.
;; - "..." sequence matching, with specifiable minimum and maximum.
;; - Multiple "..." in the same pattern.
;; - "..." works with, and the same for, every compound pattern type, i.e.,
;;   regular expression with sub-group patterns, record with field patterns,
;;   "and", "or", and "not", nested "..." patterns, and everything else.
;; - "(x ... . r)" pattern matches a possibly empty chain of pairs,
;;   like syntax-case.
;; - Clean and tractable design. syntax-case eases implementation. 
;; - The executed expanded form uses procedural abstraction instead of
;;   generating redundant code.
;; - Efficient execution.
;; - Functional, i.e., no mutation.
;; - match-lambda does the main matching logic and the other forms are defined
;;   in terms of it. This allows patterns' internal matchers to be initialized
;;   only once per match-lambda expression evaluation, which for some patterns
;;   can significantly improve efficiency when repeated calls to a match-lambda
;;   procedure are done.
;;
;; Grammar:
;; 
;; (match-lambda <clause> <clause> ...)
;; (match-lambda* <clause> <clause> ...)
;; (match <expr> <clause> <clause> ...)
;; (matches? <pat>)
;; (match-let ((<pat> <expr>) ...) <body>)
;; (match-let* ((<pat> <expr>) ...) <body>)
;; 
;; <clause> ::= (<pat> <expr>)
;;            | (<pat> <fender> <expr>)
;; <fender> ::= <expr>
;; <pat>                            Matches: 
;;  ::= _                             Anything, does not bind
;;    | <pat-var>                     Anything, bind variable
;;    | <constant>                    Datum, according to equal?
;;    | (quote <datum>)               Datum, according to equal?
;;    | (quasiquote <qq-template>)    Datum, according to equal?
;;    | ()                            Empty list
;;    | (<pat> . <pat>)               Pair
;;    | (<pat> <ooo> . <pat>)         Chain of pairs, possibly empty
;;    | #(<vec-pat> ...)              Vector
;;    | (:and <pat> ...)              If all sub-patterns match value
;;    | (:or <pat> ...)               If any sub-pattern matches value
;;    | (:not <pat>)                  If sub-pattern does not match value
;;    | (:regex <irx> <pat> ...)      String, if it matches the regular
;;                                    expression and if the captured groups
;;                                    (which are strings) match sub-patterns
;;    | (:symbol <irx> <pat> ...)     Symbol, if it matches the regular
;;                                    expression and if the captured groups
;;                                    (which are symbols) match sub-patterns
;;    | (:record <r-type> <pat> ...)  Record of specified type,
;;                                    whose fields' values match sub-patterns
;;    | (:predicate <expr>)           If result of expression applied to value
;;                                    returns true
;; <pat-var>  ::= Any <identifier> except: 
;;                ... quote quasiquote :and :or :not
;;                :regex :symbol :record :predicate
;; <constant> ::= <boolean> | <number> | <character> 
;;              | <string> | <bytevector>
;; <ooo>      ::= ... | (... <integer>) | (... <integer> <integer>)
;; <vec-pat>  ::= <pat> | <pat> <ooo>
;; <irx>      ::= <expr> which evaluates to a valid irregex for Alex Shinn's
;;                IrRegular Expressions library.
;;                I.e., a string, SRE, or compiled irregex.
;; <r-type>   ::= R6RS <record-name> handle for the record type.  This gets
;;                wrapped with record-type-descriptor.
;;              | (RTD <expr>) where the expression evaluates to a first-class
;;                record type descriptor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (xitomatl match (1 2))
  (export
    match matches?
    match-lambda match-lambda*
    match-let match-let*)
  (import
    (rnrs)
    (only (xitomatl irregex (or (0 (>= 7)) ((>= 1))))
          irregex irregex-match irregex-match-substring irregex-num-submatches)
    (only (xitomatl records)
          record-type-accessors)
    (for (only (xitomatl macro-utils)
               identifier?/name=? name=? unique-ids?/raise)
         expand)
    (for (only (xitomatl indexes)
               enumerate)
         expand)
    (for (only (xitomatl predicates)
               exact-non-negative-integer? exact-positive-integer?)
         expand))
  
  (define-syntax match-lambda
    (lambda (in-stx)
      (define (keyword? pat-stx)
        (and (identifier? pat-stx)
             (exists (lambda (x) (name=? pat-stx x)) 
                     '(quote quasiquote :and :or :not 
                       :regex :symbol :record :predicate ...))))
      (define (ooo-range-valid? ooo-stx)
        (syntax-case ooo-stx ()
          ((min max)
           (let ((min (syntax->datum #'min))
                 (max (syntax->datum #'max)))
             (and (exact-non-negative-integer? min)
                  (or (not max)
                      (and (exact-positive-integer? max)
                           (<= min max))))))))
      (define (ooo? ooo-stx)
        (syntax-case ooo-stx ()
          ((ooo min)
           (and (identifier?/name=? #'ooo '...)
                (ooo-range-valid? #'(min #F))))
          ((ooo min max)
           (and (identifier?/name=? #'ooo '...)
                (ooo-range-valid? #'(min max))))
          (ooo
           (identifier?/name=? #'ooo '...))
          (_ #F)))
      (define (ooo-range ooo-stx)
        (syntax-case ooo-stx ()
          ((_ min)     #'(min #F))
          ((_ min max) #'(min max))
          (_           #'(0 #F))))
      ;; Used for :regex and :symbol
      (define (P-regex M-obj who irx pats make-indexer)
        (with-syntax ((num-pats (length pats))
                      ((indexers ...) (map make-indexer (enumerate pats)))
                      (((M V ...) ...) (map P pats)))
          #`((let ((irx-c (irregex #,irx))
                   (l-idxrs (list indexers ...))
                   (l-m (list M ...)))
               (check-regex-patterns #,who num-pats irx-c)
               (make-matcher #,M-obj irx-c l-idxrs l-m))
             V ... ...)))
      ;; P does the core of the syntax logic.  It is given a syntax object of
      ;; a match pattern.  It returns a syntax object that is a list whose
      ;; first element is an expression which evaluates to a matcher procedure
      ;; and whose, possibly empty, remaining elements are identifiers of
      ;; pattern variables, in lexical left-to-right order, which are to be
      ;; bound by the pattern.  P is used recursively as a recursive match
      ;; pattern is parsed.
      (define (P pat-stx)
        (syntax-case pat-stx ()
          ;; empty list
          (()
           #'(M-null))
          ;; anything, ignore, don't bind
          (underscore
           (identifier?/name=? #'underscore '_)
           #'(M-ignore))
          ;; prevent misuse of pattern syntax keywords
          (invalid
           (keyword? #'invalid)
           (syntax-violation 'match "misuse of pattern syntax" in-stx pat-stx))
          ;; anything, do bind
          (var
           (identifier? #'var)
           #'(M-variable
              var))
          ;; quote'd datum
          ((q datum)
           (identifier?/name=? #'q 'quote)
           #'((make-matcher M-datum (quote datum))))
          ;; quasiquote'd datum
          ((qq datum)
           (identifier?/name=? #'qq 'quasiquote)
           #'((let ((d (quasiquote datum)))
                (make-matcher M-datum d))))
          ;; and
          ((:and pat ...)
           (identifier?/name=? #':and ':and)
           (with-syntax ((((M V ...) ...) (map P #'(pat ...))))
             #'((let ((l-m (list M ...)))
                  (make-matcher M-and l-m))
                V ... ...)))
          ;; or
          ((:or pat ...)
           (identifier?/name=? #':or ':or)
           (with-syntax ((((M V ...) ...) (map P #'(pat ...))))
             (let ((Vs #'((V ...) ...)))
               (when (positive? (length Vs))
                 (unless (let ((syms (map syntax->datum (car Vs))))
                           (for-all (lambda (x)
                                      (equal? syms (map syntax->datum x)))
                                    (cdr Vs)))
                   (syntax-violation 'match ":or pattern variables mismatch"
                                     in-stx pat-stx)))
               (with-syntax (((V ...) (if (positive? (length Vs))
                                        (car Vs)
                                        '())))
                 #'((let ((l-m (list M ...)))
                      (make-matcher M-or l-m))
                    V ...)))))
          ;; not
          ((:not pat)
           (identifier?/name=? #':not ':not)
           (with-syntax (((M V ...) (P #'pat)))
             (when (positive? (length #'(V ...)))
               (syntax-violation 'match ":not pattern contains variables"
                                 in-stx pat-stx))
             #'((let ((m M))
                  (make-matcher M-not m)))))
          ;; string, according to IrRegex regular expression
          ((:regex irx pat ...)
           (identifier?/name=? #':regex ':regex)
           (P-regex #'M-irregex ":regex"
                    #'irx #'(pat ...)
                    (lambda (i)
                      #`(lambda (m) (irregex-match-substring m #,(+ 1 i))))))
          ;; symbol, according to IrRegex regular expression
          ((:symbol irx pat ...)
           (identifier?/name=? #':symbol ':symbol)
           (P-regex #'M-symbol ":symbol"
                    #'irx #'(pat ...)
                    (lambda (i)
                      #`(lambda (m)
                          (let ((s (irregex-match-substring m #,(+ 1 i))))
                            (and s (string->symbol s)))))))
          ;; record
          ((:record rtype pat ...)
           (and (identifier?/name=? #':record ':record)
                (or (identifier? #'rtype)
                    (syntax-case #'rtype () 
                      ((RTD _) (identifier?/name=? #'RTD 'RTD) #T)
                      (_ #F))))
           (with-syntax ((rtd-expr 
                          (syntax-case #'rtype () 
                            ((_ x) #'x)
                            (x #'(record-type-descriptor x))))
                         (num-pats (length #'(pat ...)))
                         (((M V ...) ...) (map P #'(pat ...))))
             #'((let ((rtd rtd-expr)
                      (l-m (list M ...)))
                  (let ((pred (record-predicate rtd))
                        (accessors (record-type-accessors rtd)))
                    (check-record-patterns num-pats accessors)
                    (make-matcher M-record pred accessors l-m)))
                V ... ...)))
          ;; arbitrary predicate
          ((:predicate pred)
           (identifier?/name=? #':predicate ':predicate)
           #'((let ((p pred))
                (make-matcher M-predicate p))))
          ;; multiple elements of, possibly empty, chain of pairs
          ((pat ooo . pat-rest)
           (ooo? #'ooo)
           (with-syntax 
               (((ooo-M ooo-V ...) (P #'pat))
                ((min max) (ooo-range #'ooo))
                ((rest-M rest-V ...) (P #'pat-rest)))
             #`((let ((m-ooo ooo-M)
                      (m-rest rest-M))
                  (make-matcher M-pair-chain 
                                m-ooo min max 
                                (quote #,(map (lambda (_) '()) #'(ooo-V ...)))
                                m-rest))
                ooo-V ... rest-V ...)))
          ;; prevent misuse of pattern syntax keywords
          ((invalid . _)
           (keyword? #'invalid)
           (syntax-violation 'match "misuse of pattern syntax" in-stx pat-stx))
          ;; pair / list / improper list
          ((pat-car . pat-cdr)
           (with-syntax (((car-M car-V ...) (P #'pat-car))
                         ((cdr-M cdr-V ...) (P #'pat-cdr)))
             #'((let ((m-car car-M)
                      (m-cdr cdr-M))
                  (make-matcher M-pair m-car m-cdr))
                car-V ... cdr-V ...)))
          ;; multiple elements of vector
          (#(pat ...)
           (let scan ((pats #'(pat ...)) (preceded #F))
             (and (pair? pats)
                  (if (ooo? (car pats))
                    preceded
                    (scan (cdr pats) #T))))
           (let-values 
               (((pats-preceding pat-ooo min max pats-rest)
                 (let scan ((pats #'(pat ...)) 
                            (preceding '()))
                   (let ((x (car pats)))
                     (if (ooo? x)
                       (with-syntax (((min max) (ooo-range x)))
                         (values (reverse (cdr preceding))
                                 (car preceding) #'min #'max
                                 (cdr pats)))
                       (scan (cdr pats)
                             (cons x preceding)))))))
             (with-syntax 
                 ((p-len (length pats-preceding))
                  (((preceding-M preceding-V ...) ...) (map P pats-preceding))
                  ((ooo-M ooo-V ...) (P pat-ooo))
                  ((min max) (list min max))
                  ;; NOTE: the rest is matched as a list
                  ((rest-M rest-V ...) (P pats-rest)))
               #`((let ((l-m-preceding (list preceding-M ...))
                        (m-ooo ooo-M)
                        (m-rest rest-M))
                    (make-matcher M-vector-ooo
                                  p-len l-m-preceding
                                  m-ooo min max 
                                  (quote #,(map (lambda (_) '()) #'(ooo-V ...)))
                                  m-rest))
                  preceding-V ... ... ooo-V ... rest-V ...))))
          ;; vector
          (#(pat ...)
           (with-syntax ((len (length #'(pat ...)))
                         (((M V ...) ...) (map P #'(pat ...))))
             #'((let ((l-m (list M ...)))
                  (make-matcher M-vector len l-m))
                V ... ...)))
          ;; self-quoting datum
          (const
           #'((make-matcher M-datum const))))) 
      ;; start transforming
      (syntax-case in-stx () 
        ((_ clause0 clause ...)
         (with-syntax 
             ((((matcher fender-proc ... true-expr-proc) ...)
               (map (lambda (c) 
                      (syntax-case c ()
                        ((pattern fender ... true-expr)
                         (<= (length #'(fender ...)) 1)
                         (with-syntax 
                             (((M V ...) (P #'pattern)))
                           (unique-ids?/raise #'(V ...) in-stx)
                           #'(M 
                              (lambda (V ...) fender) ...
                              (lambda (V ...) true-expr))))
                        (_ (syntax-violation 'match "invalid clause" in-stx c))))
                    #'(clause0 clause ...)))
              ((m ...) (generate-temporaries #'(clause0 clause ...))))
           ;; macro output
           #'(let ((m matcher) ...)
               (lambda (obj)
                 (cond
                   ((do-matching m obj fender-proc ...)
                    => (lambda (vars) (apply true-expr-proc vars)))
                   ...
                   (else (failed-to-match obj))))))))))
  
  (define-syntax do-matching
    (syntax-rules ()
      ((_ matcher obj)
       (let ((vars (matcher obj '())))
         (and vars
              (reverse vars))))
      ((_ matcher obj fender)
       (let ((vars (matcher obj '())))
         (and vars
              (let ((vars (reverse vars)))
                (and (apply fender vars)
                     vars)))))))

  (define (AV msg . irrts)
    (apply assertion-violation 'match msg irrts))

  (define (check-regex-patterns who num-pats irx-c)
    (let ((num-subs (irregex-num-submatches irx-c)))
      (unless (= num-pats num-subs)
        (AV (string-append who " sub-patterns mismatch sub-matches")
            num-pats num-subs))))

  (define (check-record-patterns num-pats accessors)
    (let ((num-fields (length accessors)))
      (unless (= num-pats num-fields)
        (AV ":record sub-patterns mismatch fields" num-pats num-fields))))

  (define (failed-to-match obj)
    (AV "failed to match" obj))
  
  ;;------------------------------------------------------------------------
  
  (define-syntax make-matcher
    (syntax-rules ()
      ((_ M args ...)
       (lambda (obj vars)
         (M obj vars args ...)))))
  
  ;; `vars' in the below matchers is a list of the pattern variables' values,
  ;; in the reverse order the values are extracted when destructuring, i.e.,
  ;; accumulated in the order the values are extracted by cons'ing onto the
  ;; head of the list.  This is also the reverse order of the variables'
  ;; identifiers lexical occurance in the entire compound pattern.

  (define (M-null obj vars)
    (and (null? obj)
         vars))
  
  (define (M-ignore obj vars)
    vars)
  
  (define (M-variable obj vars)
    (cons obj vars))
  
  (define (M-datum obj vars datum)
    (and (equal? datum obj)
         vars))
  
  (define (M-and obj vars matchers)
    (if (null? matchers)
      vars
      (let ((vars ((car matchers) obj vars)))
        (and vars
             (M-and obj vars (cdr matchers))))))
  
  (define (M-or obj vars matchers)
    (if (null? matchers)
      #F
      (let ((next-vars ((car matchers) obj vars)))
        (or next-vars
            (M-or obj vars (cdr matchers))))))
  
  (define (M-not obj vars matcher)
    (if (matcher obj '())
      #F
      vars))
  
  (define (do-sub-matching obj procs matchers vars)
    (if (null? matchers)
      vars
      (let ((vars ((car matchers) ((car procs) obj) vars)))
        (and vars
             (do-sub-matching obj (cdr procs) (cdr matchers) vars)))))
  
  (define (M-irregex obj vars irx indexers matchers)
    (and (string? obj)
         (let ((m (irregex-match irx obj)))
           (and m
                (do-sub-matching m indexers matchers vars)))))
  
  (define (M-symbol obj vars irx indexers matchers)
    (and (symbol? obj)
         (let ((m (irregex-match irx (symbol->string obj))))
           (and m
                (do-sub-matching m indexers matchers vars)))))
  
  (define (M-record obj vars pred accessors matchers)
    (and (pred obj)
         (do-sub-matching obj accessors matchers vars)))
  
  (define (M-predicate obj vars pred)
    (and (pred obj)
         vars))
  
  (define (M-pair obj vars car-matcher cdr-matcher)
    (and (pair? obj)
         (let ((vars (car-matcher (car obj) vars)))
           (and vars
                (cdr-matcher (cdr obj) vars)))))

  (define (do-match-vector vec i matchers vars)
    (if (null? matchers)
      vars
      (let ((vars ((car matchers) (vector-ref vec i) vars)))
        (and vars
             (do-match-vector vec (+ 1 i) (cdr matchers) vars)))))
  
  (define (M-vector obj vars len matchers)
    (and (vector? obj)
         (= len (vector-length obj))
         (do-match-vector obj 0 matchers vars)))
  
  (define (M-pair-chain obj vars 
                        ooo-matcher min max 
                        empty-ooo-vars
                        rest-matcher)
    ;; In order to match rest-matcher against the end of the chain, we must
    ;; work backwards across the chain of pairs (otherwise it might match
    ;; before the end).  So we create a list of the pairs in reverse order
    ;; and use that.  This is more effecient than the non-tail-recursive
    ;; solution of a function which immediately recurs forwards across the
    ;; chains and does the backwards work as the recursive calls return.  The
    ;; stack space used by that solution significantly exceeds that of the
    ;; space used by the reversed chain list, and that solution does not 
    ;; return as a tail-return because it must test each recursive call's 
    ;; return value, which costs significantly more time.
    (let match-last ((rev (let reverse-chain ((obj obj) (rev '()))
                            (if (pair? obj)
                              (reverse-chain (cdr obj) (cons obj rev))
                              (cons obj rev)))))
      (and (pair? rev)
           (let ((rest-vars (rest-matcher (car rev) '())))
             (if rest-vars
               (let match-ooo ((rev (cdr rev))
                               (accum-ooo-vars empty-ooo-vars)
                               (count 0))
                 (if (pair? rev)
                   (and (or (not max) 
                            (< count max))
                        (let ((ooo-vars (ooo-matcher (caar rev) '())))
                          (and ooo-vars
                               (match-ooo (cdr rev)
                                          (map cons ooo-vars accum-ooo-vars)
                                          (+ 1 count)))))
                   (and (>= count min)
                        (append rest-vars
                                accum-ooo-vars
                                vars))))
               (match-last (cdr rev)))))))
  
  (define (M-vector-ooo obj vars
                        p-len preceding-matchers
                        ooo-matcher min max
                        empty-ooo-vars
                        rest-matcher)
    (and (vector? obj)
         (let ((obj-len (vector-length obj)))
           (and (>= obj-len p-len)
                (let ((vars (do-match-vector obj 0 preceding-matchers vars)))
                  (and vars
                       (let match-last ((last '()) 
                                        (y (- obj-len 1)))
                         (let ((rest-vars (rest-matcher last '())))
                           (if rest-vars
                             (let match-ooo ((x y)
                                             (accum-ooo-vars empty-ooo-vars)
                                             (count 0))
                               (if (>= x p-len)
                                 (and (or (not max) 
                                          (< count max))
                                      (let ((ooo-vars
                                             (ooo-matcher (vector-ref obj x) '())))
                                        (and ooo-vars
                                             (match-ooo (- x 1)
                                                        (map cons ooo-vars accum-ooo-vars)
                                                        (+ 1 count)))))
                                 (and (>= count min)
                                      (append rest-vars
                                              accum-ooo-vars
                                              vars))))
                             (and (>= y p-len)
                                  (match-last (cons (vector-ref obj y) last)
                                              (- y 1))))))))))))
  
  ;;------------------------------------------------------------------------

  (define-syntax match
    (syntax-rules ()
      ((_ expr clause0 clause ...)
       ((match-lambda clause0 clause ...) expr))))
  
  (define-syntax matches?
    (syntax-rules ()
      ((_ pattern)
       (match-lambda (pattern #T) (_ #F)))))
  
  (define-syntax match-lambda*
    (syntax-rules ()
      ((_ clause ...)
       (let ((m (match-lambda clause ...)))
         (lambda x (m x))))))
  
  (define-syntax match-let
    (syntax-rules ()
      ((_ () body0 body ...)
       (let () body0 body ...))
      ((_ ((pat expr)) body0 body ...) 
       (match expr
         (pat
          (let () body0 body ...))))
      ((_ ((pat expr) ...) body0 body ...) 
       (match (vector expr ...) 
         (#(pat ...) 
          (let () body0 body ...))))))
  
  (define-syntax match-let*
    (syntax-rules ()
      ((_ () body0 body ...)
       (let () body0 body ...))
      ((_ ((pat0 expr0) (pat expr) ...) body0 body ...)
       (match expr0 
         (pat0 
          (match-let* ((pat expr) ...) body0 body ...))))))

)
