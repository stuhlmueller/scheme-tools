#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl ssax private-5-1 util)
  (export
    ;; Only the ones (xitomatl ssax ---) and (xitomatl sxml-tools ---) need
    string->integer
    string-split
    make-char-quotator
    substring?
    string-whitespace?)
  (import
    (except (rnrs) error)
    (rnrs mutable-pairs)
    (xitomatl include)
    (xitomatl ssax private-5-1 error)
    (xitomatl ssax private-5-1 misc)
    (except (srfi :13 strings) 
            string-copy string-for-each string->list
            string-upcase string-downcase string-titlecase string-hash))
  
  (define error (make-errorer "(xitomatl ssax private-5-1 util)"))

  ; Test if a string is made of only whitespace
  ; An empty string is considered made of whitespace as well
  (define (string-whitespace? str)
    (let ((len (string-length str)))
      (cond
        ((zero? len) #T)
        ((= 1 len) (char-whitespace? (string-ref str 0)))
        ((= 2 len) (and (char-whitespace? (string-ref str 0))
                        (char-whitespace? (string-ref str 1))))
        (else
         (let loop ((i 0))
           (or (>= i len)
               (and (char-whitespace? (string-ref str i))
                    (loop (inc i)))))))))
  
  
  (include/resolve ("xitomatl" "ssax" "private-5-1") "util.scm")
)
