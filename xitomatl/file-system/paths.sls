#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl file-system paths)
  (export
    path-style
    path? absolute-path? relative-path? path=?
    path-join path-split normalize-path)
  (import
    (rnrs)
    (only (srfi :39 parameters) make-parameter)
    (only (xitomatl define) define/AV)
    (xitomatl feature-cond)
    (for (only (xitomatl macro-utils) with-syntax* identifier-append) expand)
    (only (xitomatl strings) string-intersperse string-split))

  (define/AV path-style
    (make-parameter (feature-cond (posix 'posix) (windows 'windows))
                    (lambda (x)
                      (if (and (symbol? x)
                               (memq x '(posix windows windows-/)))
                        x
                        (AV "not a symbol, or unknown" x)))))

  (define (split-drive p)
    (let ((len (string-length p)))
      (let loop ((i 0))
        (cond ((= len i) (values #F (and (< 0 len) p)))
              ((and (< 0 i) (char=? #\: (string-ref p i)))
               (let ((i (+ 1 i)))
                 (values (substring p 0 i)
                         (and (< i len) (substring p i len)))))
              (else (loop (+ 1 i)))))))
  (define (drive p) (let-values (((d r) (split-drive p))) d))
  (define (no-drive p) (let-values (((d r) (split-drive p))) r))

  (define-syntax define/styles
    (lambda (stx)
      (syntax-case stx ()
        ((_ (name args ...) (style . body) ...)
         (with-syntax*
             ((((style . body) ...)
               (apply append
                      (map (lambda (x)
                             (syntax-case x ()
                               (((s ...) . b) #'((s . b) ...))
                               (_ (list x))))
                           #'((style . body) ...))))
              ((name--style ...)
               (map (lambda (s) (identifier-append #'name #'name "--" s))
                    #'(style ...)))
              (((renames ...) ...)
               (map (lambda (s)
                      (map (lambda (x)
                             `(,(datum->syntax #'name x)
                               (,#'identifier-syntax
                                ,(identifier-append #'name x "--" s))))
                           '(sep rooted-path? path? absolute-path?
                             relative-path? _path=? normalize-path
                             _path-join-help _path-join path-split)))
                    #'(style ...))))
           #'(begin
               (define (name args ...)
                 (case (path-style)
                   ((style) (name--style args ...))
                   ...))
               (let-syntax (renames ...)
                 (define (name--style args ...) . body))
               ...))))))

  (define/styles (sep) ((posix windows-/) "/") (windows "\\"))

  (define/styles (rooted-path? p)
    (posix (char=? #\/ (string-ref p 0)))
    (windows (char=? #\\ (string-ref (no-drive p) 0)))
    (windows-/ (char=? #\/ (string-ref (no-drive p) 0))))

  (define/styles (path? x)
    (posix (and (string? x) (positive? (string-length x))))
    ((windows windows-/) (and (string? x) (and (no-drive x) #T))))

  (define/styles (absolute-path? x)
    ((posix windows windows-/) (and (path? x) (rooted-path? x))))

  (define/styles (relative-path? x)
    ((posix windows windows-/) (and (path? x) (not (rooted-path? x)))))

  (define/styles (_path=? x y r)
    ((posix windows windows-/) (apply string=? (map normalize-path (cons* x y r)))))

  (define (path=? x y . r) (_path=? x y r))

  (define/styles (normalize-path p)
    ((posix windows windows-/) (_path-join (path-split p))))

  (define/styles (_path-join-help c1 c2)
    (posix
     (if (and (pair? c1) (absolute-path? (car c1)))
       (if (pair? c2) (cons "" c2) (list "" ""))
       c2))
    ((windows windows-/)
     (if (and (pair? c1) (absolute-path? (car c1)))
       (if (drive (car c1))
         (if (pair? (cdr c2)) c2 (list (car c2) ""))
         (if (pair? c2) (cons "" c2) (list "" "")))
       c2)))

  (define/styles (_path-join components)
    ((posix windows windows-/)
     (let* ((s (sep))
            (c1 (filter (lambda (x) (positive? (string-length x))) components))
            (c2 (apply append (map (lambda (x) (string-split x s #F)) c1))))
       (string-intersperse (_path-join-help c1 c2) s))))

  (define (path-join . components) (_path-join components))

  (define/styles (path-split p)
    (posix
     (let* ((s (sep))
            (c (string-split p s #F)))
       (if (absolute-path? p) (cons s c) c)))
    ((windows windows-/)
     (let* ((s (sep))
            (c (string-split p s #F)))
       (if (absolute-path? p)
         (if (and (pair? c) (drive (car c)))
           (cons (string-append (car c) s) (cdr c))
           (cons s c))
         c))))
)
