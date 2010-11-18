#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl strings)
  (export
    string-intersperse
    string-split whitespace
    string-end=?
    ;; from (xitomatl strings compat)
    string-copy!)
  (import
    (rnrs)
    (only (xitomatl lists) intersperse)
    (xitomatl strings compat))
  
  (define (string-intersperse sl ssep)
    (apply string-append (intersperse sl ssep)))
  
  (define whitespace 
    (apply string
           '(#\space #\linefeed #\return #\tab #\vtab #\page #\x85 #\xA0 
             #\x1680 #\x180E #\x2000 #\x2001 #\x2002 #\x2003 #\x2004 #\x2005
             #\x2006 #\x2007 #\x2008 #\x2009 #\x200A #\x2028 #\x2029 #\x202F
             #\x205F #\x3000)))
  
  (define string-split
    (case-lambda
      ((str) 
       (string-split str whitespace #F))
      ((str delim-chars)
       (string-split str delim-chars #F))
      ((str delim-chars keep-empty)
       (unless (and (string? str) (string? delim-chars))
         (assertion-violation 'string-split "not a string" 
                              (if (string? delim-chars) str delim-chars)))
       (let ((strlen (string-length str))
             (dellen (string-length delim-chars)))
         (let loop ((i (- strlen 1))
                    (to strlen)
                    (accum '()))
           (if (< i 0)
             (if (or (< 0 to) keep-empty)
               (cons (substring str 0 to) accum)
               accum)
             (let ((c (string-ref str i)))
               (let check ((j 0))
                 (cond ((= j dellen) (loop (- i 1) to accum))
                       ((char=? c (string-ref delim-chars j))
                        (loop (- i 1) i (let ((i+1 (+ i 1)))
                                          (if (or (< i+1 to) keep-empty)
                                            (cons (substring str i+1 to) accum)
                                            accum))))
                       (else (check (+ j 1))))))))))))
  
  (define (string-end=? str end)
    (let ((sl (string-length str))
          (el (string-length end)))
      (and (>= sl el)
           (string=? (substring str (- sl el) sl) end))))
)
