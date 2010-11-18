#!r6rs
;; Copyright 2010 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (srfi :78 lightweight-testing)
  (only (xitomatl enumerators) fold/enumerator)
  (only (xitomatl lists) sublist)
  (xitomatl irregex)
  (xitomatl irregex extras))

(define-syntax check-ex/not-advancing
  (syntax-rules ()
    ((_ expr)
     (check (guard (ex (else (and (assertion-violation? ex)
                                  (message-condition? ex)
                                  (condition-message ex))))
              expr
              'unexpected-return)
            => "pattern not advancing search"))))

(define-syntax check-AV
  (syntax-rules ()
    ((_ expr)
     (check (guard (ex (else (assertion-violation? ex)))
              expr
              'unexpected-return)
            => #T))))

;;----------------------------------------------------------------------------

(define text0 "This is a sentence.")

;;;; irregex-search/all

(check (irregex-search/all "foobar" text0)
       => '())
(check (let ((ms (irregex-search/all "\\w+" text0)))
         (and (for-all irregex-match-data? ms)
              (length ms)))
       => 4)
(check (map irregex-match-substring
            (irregex-search/all "\\w+" text0))
       => '("This" "is" "a" "sentence"))
(check (map (lambda (m) (irregex-match-substring m 1))
            (irregex-search/all "\\w+\\s+(\\w+)" text0))
       => '("is" "sentence"))
(check-ex/not-advancing (irregex-search/all "" text0))
(check-ex/not-advancing (irregex-search/all ".*" text0))
(check-ex/not-advancing (irregex-search/all "x*?" text0))

;;;; irregex-search/all/strings

(check (irregex-search/all/strings "foobar" text0)
       => '())
(check (irregex-search/all/strings "\\w+" text0)
       => '("This" "is" "a" "sentence"))
(check (irregex-search/all/strings "\\w+\\s+(\\w+)" text0)
       => '("This is" "a sentence"))
(check-ex/not-advancing (irregex-search/all/strings "" text0))
(check-ex/not-advancing (irregex-search/all/strings ".*" text0))
(check-ex/not-advancing (irregex-search/all/strings "x*?" text0))

;;----------------------------------------------------------------------------

;;;; make-lose-refs chunk-eqv? chunk-equal?
;; list-chunking-lose-refs uses make-lose-refs

(let* ((chunk (map string-copy ; newly-allocated pairs and strings
                   '("This" "is" "a" "test" "of" "losing" "refs")))
       (saved (let loop ((x chunk) (a '()))
                (if (null? x)
                  (reverse a)
                  (loop (cdr x) (cons (list x (car x) (cdr x)) a)))))
       (submatch-chunks (list (list 0 chunk (cddddr chunk))
                              (list 1 (cdr chunk) (cdddr chunk))
                              (list 2 (cddr chunk) (cddddr chunk))
                              (list 4 (cddddr chunk) (cddddr chunk))))
       (group car) (start cadr) (end caddr)
       (replacements (list-chunking-lose-refs submatch-chunks)))
  ;; original unaltered
  (check (let loop ((x chunk) (y saved))
           (and (eq? x (caar y))
                (eq? (car x) (cadar y))
                (eq? (cdr x) (caddar y))
                (or (and (null? (cdr x)) (null? (cdr y)))
                    (loop (cdr x) (cdr y)))))
         => #T)
  (check chunk => '("This" "is" "a" "test" "of" "losing" "refs"))
  ;; replacements correct
  (check (length replacements) => (length submatch-chunks))
  (check (exists eq? replacements submatch-chunks)
         => #F)
  (check (for-all (lambda (x y)
                    (and (= (group x) (group y))
                         (not (eq? (start x) (start y)))
                         (eq? (car (start x)) (car (start y)))
                         (not (eq? (end x) (end y)))
                         (eq? (car (end x)) (car (end y)))))
                  replacements submatch-chunks)
         => #T)
  (check (for-all (lambda (x y)
                    (and ((chunk-eqv? list-chunker) (start x) (start y))
                         ((chunk-eqv? list-chunker) (end x) (end y))))
                  replacements submatch-chunks)
         => #T)
  (check (for-all (lambda (x y)
                    (and ((chunk-equal? list-chunker) (start x) (start y))
                         ((chunk-equal? list-chunker) (end x) (end y))))
                  replacements submatch-chunks)
         => #T)
  (check (start (car replacements)) => '("This" "is" "a" "test" "of"))
  (check (end (car replacements)) => '("of"))
  (check (start (cadr replacements)) => '("is" "a" "test" "of"))
  (check (end (cadr replacements)) => '("test" "of"))
  (check (start (caddr replacements)) => '("a" "test" "of"))
  (check (end (caddr replacements)) => '("of"))
  (check (start (cadddr replacements)) => '("of"))
  (check (end (cadddr replacements)) => '("of"))
  (let* ((get-next (chunker-get-next list-chunker))
         (last-chunk
          (lambda (ic)
            (let loop ((c ic))
              (let ((n (get-next c)))
                (if n (loop n) c))))))
    (check (last-chunk (start (car replacements)))
           (=> eq?) (end (car replacements))))
  (let* ((get-next (chunker-get-next list-chunker))
         (chain-list
          (lambda (ic)
            (let loop ((c ic) (a '()))
              (if c
                (loop (get-next c) (cons c a))
                (reverse a)))))
         (chain-list (chain-list (start (car replacements)))))
    (check (for-all (lambda (x)
                      (and (memq (start x) chain-list)
                           (memq (end x) chain-list)
                           #T))
                    replacements)
           => #T))
  (check (eq? (end (car replacements)) (end (caddr replacements))) => #T)
  (check (eq? (end (caddr replacements)) (start (cadddr replacements))) => #T)
  (check (eq? (start (cadddr replacements)) (end (cadddr replacements))) => #T)
  (let ((except
         (lambda (l i)
           (let ((l (apply append (map cdr l))))
             (append (sublist l 0 i) (sublist l (+ 1 i)))))))
    (check (memq (start (list-ref replacements 0)) (except replacements 0)) => #F)
    (check (memq (start (list-ref replacements 1)) (except replacements 2)) => #F)
    (check (memq (end (list-ref replacements 1)) (except replacements 3)) => #F)
    (check (memq (start (list-ref replacements 2)) (except replacements 4)) => #F)))

;;----------------------------------------------------------------------------

(define chunked-text0
  '("Once u" "" "p" "on " "" "a time." "." ". "
    " There was a string " "used for tes" "ting ch" "unks!" ""))

;;;; irregex-search/chunked/all

(check (irregex-search/chunked/all "foobar" list-chunker '(""))
       => '())
(check (irregex-search/chunked/all "foobar" list-chunker (list "")
                                   list-chunking-lose-refs)
       => '())
(check (irregex-search/chunked/all "foobar" list-chunker '("" "" "" ""))
       => '())
(check (irregex-search/chunked/all "foobar" list-chunker (list "" "" "" "")
                                   list-chunking-lose-refs)
       => '())
(check (irregex-search/chunked/all "foobar" list-chunker chunked-text0)
       => '())
(check (irregex-search/chunked/all "foobar" list-chunker chunked-text0
                                   list-chunking-lose-refs)
       => '())
(check (let ((ms (irregex-search/chunked/all "\\w+" list-chunker chunked-text0)))
         (and (for-all irregex-match-data? ms)
              (length ms)))
       => 12)
(check (let ((ms (irregex-search/chunked/all "\\w+" list-chunker chunked-text0
                                             list-chunking-lose-refs)))
         (and (for-all irregex-match-data? ms)
              (length ms)))
       => 12)
(check (map irregex-match-substring
            (irregex-search/chunked/all "\\w+" list-chunker chunked-text0))
       => '("Once" "upon" "a" "time"
            "There" "was" "a" "string" "used" "for" "testing" "chunks"))
(check (map irregex-match-substring
            (irregex-search/chunked/all "\\w+" list-chunker chunked-text0
                                        list-chunking-lose-refs))
       => '("Once" "upon" "a" "time"
            "There" "was" "a" "string" "used" "for" "testing" "chunks"))
(check (map (lambda (m) (irregex-match-substring m 1))
            (irregex-search/chunked/all "[Oo](\\w)" list-chunker chunked-text0))
       => '("n" "n" "r"))
(check (map (lambda (m) (irregex-match-substring m 1))
            (irregex-search/chunked/all "[Oo](\\w)" list-chunker chunked-text0
                                        list-chunking-lose-refs))
       => '("n" "n" "r"))
(check (map (lambda (m) (irregex-match-substring m 4))
            (irregex-search/chunked/all "(e)((\\w+)(e))" list-chunker chunked-text0))
       => '("e"))
(check (map (lambda (m) (irregex-match-substring m 4))
            (irregex-search/chunked/all "(e)((\\w+)(e))" list-chunker chunked-text0
                                        list-chunking-lose-refs))
       => '("e"))
(check-ex/not-advancing
 (map irregex-match-substring
      (irregex-search/chunked/all "^.*$" list-chunker chunked-text0)))
(check-ex/not-advancing
 (map irregex-match-substring
      (irregex-search/chunked/all "^.*$" list-chunker chunked-text0
                                  list-chunking-lose-refs)))
(check (irregex-search/chunked/all "(?:(foo)|(bar))\\s*zab" list-chunker
                                   '("bar " " zab" "fo" "oza" "b")
                                   list-chunking-lose-refs
                                   (lambda (m)
                                     (list (irregex-match-substring m 0)
                                           (irregex-match-substring m 1)
                                           (irregex-match-substring m 2))))
       => '(("bar  zab" #F "bar") ("foozab" "foo" #F)))

;;;; irregex-search/chunked/all/strings

(check (irregex-search/chunked/all/strings "foobar" list-chunker '(""))
       => '())
(check (irregex-search/chunked/all/strings "foobar" list-chunker '("" "" "" ""))
       => '())
(check (irregex-search/chunked/all/strings "foobar" list-chunker chunked-text0)
       => '())
(check (let ((ms (irregex-search/chunked/all/strings "\\w+" list-chunker chunked-text0)))
         (and (for-all string? ms)
              (length ms)))
       => 12)
(check (irregex-search/chunked/all/strings "\\w+" list-chunker chunked-text0)
       => '("Once" "upon" "a" "time"
            "There" "was" "a" "string" "used" "for" "testing" "chunks"))
(check (irregex-search/chunked/all/strings "[Oo](\\w)" list-chunker chunked-text0)
       => '("On" "on" "or"))
(check (irregex-search/chunked/all/strings "(e)((\\w+)(e))" list-chunker chunked-text0)
       => '("ere"))
(check-ex/not-advancing
 (irregex-search/chunked/all/strings "^.*$" list-chunker chunked-text0))

;;----------------------------------------------------------------------------

;;;; range-list-chunker range-list-chunking-lose-refs

(define chunked-text1
  '("xAax" 1 3 "Bb" 0 2 "xCc" 1 3 "Ddx" 0 2 "xxEe" 2 4 "Ffxx" 0 2 "Gg" 0 2
    "xxHhxx" 2 4 "" 0 0 "Ii" 0 2 "Jj" 0 2 "xxxKkx" 3 5 "Ll" 0 2 "Mm" 0 2
    "Nn" 0 2 "xOoxxx" 1 3 "Pp" 0 2 "Qq" 0 2 "Rr" 0 2 "Ss" 0 2 "Tt" 0 2
    "Uu" 0 2 "Vv" 0 2 "xxxxxWwxxx" 5 7 "Xx" 0 2 "" 0 0 "Yy" 0 2 "Zz" 0 2))

(check (irregex-search/chunked/all "foobar" range-list-chunker chunked-text1)
       => '())
(check (irregex-search/chunked/all "foobar" range-list-chunker chunked-text1
                                   range-list-chunking-lose-refs)
       => '())
(check (map irregex-match-substring
            (irregex-search/chunked/all "(?i:w+)" range-list-chunker chunked-text1))
       => '("Ww"))
(check (map irregex-match-substring
            (irregex-search/chunked/all "(?i:w+)" range-list-chunker chunked-text1
                                        range-list-chunking-lose-refs))
       => '("Ww"))
(check (map irregex-match-substring
            (irregex-search/chunked/all "Bb.*Gg" range-list-chunker chunked-text1))
       => '("BbCcDdEeFfGg"))
(check (map irregex-match-substring
            (irregex-search/chunked/all "Bb.*Gg" range-list-chunker chunked-text1
                                        range-list-chunking-lose-refs))
       => '("BbCcDdEeFfGg"))
(check (map irregex-match-substring
            (irregex-search/chunked/all ".{4}" range-list-chunker chunked-text1))
       => '("AaBb" "CcDd" "EeFf" "GgHh" "IiJj" "KkLl" "MmNn" "OoPp"
            "QqRr" "SsTt" "UuVv" "WwXx" "YyZz"))
(check (map irregex-match-substring
            (irregex-search/chunked/all ".{4}" range-list-chunker chunked-text1
                                        range-list-chunking-lose-refs))
       => '("AaBb" "CcDd" "EeFf" "GgHh" "IiJj" "KkLl" "MmNn" "OoPp"
            "QqRr" "SsTt" "UuVv" "WwXx" "YyZz"))

;;----------------------------------------------------------------------------

(check-AV (make-port-chunker 'oops))
(check-AV (make-port-chunker 0))
(check-AV (make-port-chunker -1))

(define (make-sip)
  (open-string-input-port
   "The best way to implement the future is to avoid having to predict it."))

;;;; irregex-search-port/all

(check (map irregex-match-substring
            (irregex-search-port/all "([Tt]o).*?\\1" (make-sip)))
       => '("to implement the future is to"))
(check (map irregex-match-substring
            (irregex-search-port/all "\\s" (make-sip)))
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check (irregex-search-port/all "([Tt]o).*?\\1" (make-sip) irregex-match-substring 1)
       => '("to implement the future is to"))
(check (irregex-search-port/all "\\s" (make-sip) irregex-match-substring 1)
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check (irregex-search-port/all "([Tt]o).*?\\1" (make-sip) irregex-match-substring 2)
       => '("to implement the future is to"))
(check (irregex-search-port/all "\\s" (make-sip)  irregex-match-substring 2)
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check (irregex-search-port/all "([Tt]o).*?\\1" (make-sip) irregex-match-substring 7)
       => '("to implement the future is to"))
(check (irregex-search-port/all "\\s" (make-sip) irregex-match-substring 7)
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check (irregex-search-port/all "([Tt]o).*?\\1" (make-sip) irregex-match-substring #e1e4)
       => '("to implement the future is to"))
(check (irregex-search-port/all "\\s" (make-sip) irregex-match-substring #e1e4)
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check-ex/not-advancing (irregex-search-port/all "" (make-sip)))
(check-ex/not-advancing (irregex-search-port/all "" (make-sip) values 1))
(check-ex/not-advancing (irregex-search-port/all "" (make-sip) values 4))
(check-ex/not-advancing (irregex-search-port/all "" (make-sip) values 29))
(check-ex/not-advancing (irregex-search-port/all "" (make-sip) values 100))
(check-ex/not-advancing (irregex-search-port/all "x*y*" (make-sip)))
(check-ex/not-advancing (irregex-search-port/all "x*y*" (make-sip) values 5))
(check-ex/not-advancing (irregex-search-port/all "x*y*" (make-sip) values 18))
(check-ex/not-advancing (irregex-search-port/all "x*y*" (make-sip) values 40))
(check-ex/not-advancing (irregex-search-port/all "x*y*" (make-sip) values #e2e4))

;;;; irregex-search-port/all/strings

(check (irregex-search-port/all/strings "([Tt]o).*?\\1" (make-sip))
       => '("to implement the future is to"))
(check (irregex-search-port/all/strings "\\s" (make-sip))
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check (irregex-search-port/all/strings "([Tt]o).*?\\1" (make-sip) 1)
       => '("to implement the future is to"))
(check (irregex-search-port/all/strings "\\s" (make-sip) 1)
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check (irregex-search-port/all/strings "([Tt]o).*?\\1" (make-sip) 2)
       => '("to implement the future is to"))
(check (irregex-search-port/all/strings "\\s" (make-sip) 2)
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check (irregex-search-port/all/strings "([Tt]o).*?\\1" (make-sip) 7)
       => '("to implement the future is to"))
(check (irregex-search-port/all/strings "\\s" (make-sip) 7)
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check (irregex-search-port/all/strings "([Tt]o).*?\\1" (make-sip) #e1e4)
       => '("to implement the future is to"))
(check (irregex-search-port/all/strings "\\s" (make-sip) #e1e4)
       => '(" " " " " " " " " " " " " " " " " " " " " " " " " "))
(check-ex/not-advancing (irregex-search-port/all/strings "" (make-sip)))
(check-ex/not-advancing (irregex-search-port/all/strings "" (make-sip) 1))
(check-ex/not-advancing (irregex-search-port/all/strings "" (make-sip) 2))
(check-ex/not-advancing (irregex-search-port/all/strings "" (make-sip) 7))
(check-ex/not-advancing (irregex-search-port/all/strings "" (make-sip) #e1e4))
(check-ex/not-advancing (irregex-search-port/all/strings "x*y*" (make-sip)))
(check-ex/not-advancing (irregex-search-port/all/strings "x*y*" (make-sip) 3))
(check-ex/not-advancing (irregex-search-port/all/strings "x*y*" (make-sip) 5))
(check-ex/not-advancing (irregex-search-port/all/strings "x*y*" (make-sip) 13))
(check-ex/not-advancing (irregex-search-port/all/strings "x*y*" (make-sip) 321))

;; All the above procedures currently use the enumerators internally, so they test
;; most of the enumerators' logic.  The below only tests what the above has not.

;;;; enumerators

(check (fold/enumerator (irregex-string-enumerator "(\\w+)-\\w+")
                        "this-will-search-until-stop-and-this-won't-be-seen"
                        (lambda (m)
                          (let ((s (irregex-match-substring m 1)))
                            (or (not (string=? s "stop"))
                                (values #F s)))))
       => "stop")
(check-ex/not-advancing
 (fold/enumerator (irregex-string-enumerator "^.*$")
                  "abcdefghijklmnopqrstuvwxyz"
                  (lambda (m a)
                    (values #T (cons (irregex-match-substring m) a)))
                  '()))
(check (fold/enumerator (irregex-string-enumerator "^.*$" 3 9)
                        "abcdefghijklmnopqrstuvwxyz"
                        (lambda (m a)
                          (let ((s (irregex-match-substring m)))
                            (if (positive? (string-length s))
                              (values #T (cons s a))
                              (values #F (cons s a)))))
                        '())
       => '("" "defghi"))
(check (fold/enumerator (irregex-chunk-enumerator "." list-chunker)
                        '("zabbo")
                        (lambda (_) (values #F 'ok)))
       => 'ok)
(check (fold/enumerator (irregex-chunk-enumerator ".*?" list-chunker)
                        '("zabbo")
                        (lambda (m) (values #F (irregex-match-substring m))))
       => "")
(check-ex/not-advancing
 (fold/enumerator (irregex-chunk-enumerator ".*?" list-chunker)
                  '("zabbo")
                  (lambda (_) #T)))
(check (fold/enumerator (irregex-chunk-enumerator ".*?" list-chunker)
                        '("zabbo")
                        (lambda (m)
                          (if (string=? "" (irregex-match-substring m))
                            (values #F 'stopped)
                            #T)))
       => 'stopped)
(check (fold/enumerator (irregex-list-enumerator ".")
                        chunked-text0
                        (lambda (m i) (values #T (+ 1 i)))
                        0)
       => 64)
(check (fold/enumerator (irregex-list-enumerator "")
                        '()
                        (lambda (_) (assert #F))
                        'ok)
       => 'ok)
(check (fold/enumerator (irregex-port-enumerator "\\w{8,}")
                        (make-sip)
                        (lambda (m a) (values #T (cons (irregex-match-substring m) a)))
                        '())
       => '("implement"))
(check (fold/enumerator (irregex-port-enumerator "\\w{8,}" 3)
                        (make-sip)
                        (lambda (m a) (values #T (cons (irregex-match-substring m) a)))
                        '())
       => '("implement"))
(check (fold/enumerator (irregex-enumerator "i.")
                        "generic is convenient"
                        (lambda (m x) (values #T (irregex-match-substring m)))
                        #F)
       => "ie")
(check (fold/enumerator (irregex-enumerator "i.")
                        '("IiIi" "Fooi" "B")
                        (lambda (m a) (values #T (cons (irregex-match-substring m) a)))
                        '())
       => '("iB" "iF" "iI"))
(check (fold/enumerator (irregex-enumerator "i.")
                        (make-sip)
                        (lambda (m i)
                          (if (< i 3)
                            (values #T (+ 1 i))
                            (values #F (irregex-match-substring m))))
                        0)
       => "in")
(check-AV (fold/enumerator (irregex-enumerator "") 'bad-type (lambda _ (raise 'bork))))


(check-report)
