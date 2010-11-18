#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl include)
  (export
    include
    include/lexical-context
    include/resolve)
  (import 
    (rnrs)
    (only (xitomatl include compat) stale-when)
    (for (only (xitomatl include compat) read-annotated search-paths) expand)
    (for (only (xitomatl file-system base) file-mtime) run expand)
    (for (only (xitomatl file-system paths) path-join path?) expand)
    (for (only (xitomatl exceptions) error/conditions) expand))
  
  (define-syntax include
    (lambda (stx)
      (syntax-case stx ()
        ((ctxt filename)
         #'(include/lexical-context ctxt filename)))))
  
  (define-syntax include/lexical-context
    (lambda (stx)
      (syntax-case stx ()
        ((_ ctxt filename)
         (and (or (identifier? #'ctxt)
                  (syntax-violation #F "not an identifier" stx #'ctxt))
              (or (path? (syntax->datum #'filename))
                  (syntax-violation #F "not a path" stx #'filename)))
         (let* ((fn (syntax->datum #'filename))
                (datums 
                 (with-exception-handler
                   (lambda (ex)
                     (error/conditions 'include/lexical-context
                      "error while trying to include" (list fn)
                      (if (condition? ex) ex (make-irritants-condition (list ex)))))
                   (lambda ()
                     (call-with-input-file fn
                       (lambda (fip)
                         (let loop ((a '()))
                           (let ((x (read-annotated fip)))
                             (if (eof-object? x)
                               (reverse a)
                               (loop (cons x a)))))))))))
           #`(stale-when
                (or (not (file-exists? #,fn))
                    (> (file-mtime #,fn) #,(file-mtime fn)))
              . #,(datum->syntax #'ctxt datums)))))))
  
  (define-syntax include/resolve
    (lambda (stx)
      (syntax-case stx ()
        ((ctxt (lib-path* ...) file-path)
         (for-all path? (syntax->datum #'(lib-path* ... file-path)))
         (let ((p (apply path-join (syntax->datum #'(lib-path* ... file-path))))
               (sp (search-paths)))
           (let loop ((search sp))
             (if (null? search)
               (error 'include/resolve "cannot find file in search paths" p sp)
               (let ((full (path-join (car search) p)))
                 (if (file-exists? full)
                   #`(include/lexical-context ctxt #,full)
                   (loop (cdr search)))))))))))
)
