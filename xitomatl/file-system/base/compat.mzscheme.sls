#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl file-system base compat)
  (export
    directory-enumerator directory-list
    current-directory delete-file delete-directory
    make-symbolic-link make-directory change-mode file-mtime file-ctime
    file-exists? file-regular? file-directory? file-symbolic-link?
    file-readable? file-writable? file-executable? file-size rename-file)
  (import
    (except (rnrs) file-exists?)
    (prefix (only (scheme base) 
                  link-exists? directory-exists? path->string make-directory
                  directory-list delete-directory file-exists? 
                  make-file-or-directory-link current-directory
                  file-or-directory-modify-seconds file-or-directory-permissions
                  file-size rename-file-or-directory
                  with-handlers exn:fail:filesystem? exn-message
                  make-parameter) 
            mz:)
    (prefix (only (scheme mpair) list->mlist) mz:)
    (only (xitomatl predicates) exact-non-negative-integer?))
  
  (define-syntax not-implemented
    (syntax-rules ()
      ((_ id ...)
       (begin
         (define (id . _) (assertion-violation 'id "not implemented"))
         ...))))
  
  (not-implemented directory-enumerator change-mode file-ctime)
  
  (define (reraise/io-f who path)
    (lambda (exn) 
      (raise (condition (make-who-condition who)
                        (make-message-condition (mz:exn-message exn))
                        (make-i/o-filename-error path)))))
  
  (define current-directory
    (mz:make-parameter (mz:path->string (mz:current-directory))
     (lambda (x)
       (mz:current-directory x)
       (mz:path->string (mz:current-directory)))))
  
  (define (directory-list path)
    (map mz:path->string 
         (mz:list->mlist
          (mz:with-handlers ((mz:exn:fail:filesystem? (reraise/io-f 'directory-list path)))
            (mz:directory-list path)))))
  
  (define delete-directory
    (case-lambda
      ((path) 
       (delete-directory path #F))
      ((path want-error)
       (mz:with-handlers ((mz:exn:fail:filesystem? 
                           (if want-error
                             (reraise/io-f 'delete-directory path)
                             (lambda (_) #F))))
         (mz:delete-directory path)
         (if want-error (values) #T)))))
  
  (define (make-symbolic-link to path)
    (mz:with-handlers ((mz:exn:fail:filesystem? (reraise/io-f 'make-symbolic-link path)))
      (mz:make-file-or-directory-link to path)))
  
  (define make-directory
    (case-lambda 
      ((path) 
       (mz:with-handlers ((mz:exn:fail:filesystem? (reraise/io-f 'make-directory path)))
         (mz:make-directory path)))
      ((path mode)
       (make-directory path)
       (change-mode path mode))))
  
  #;(define (change-mode path mode)
    #|Use MzScheme's FFI to use C chmod, I guess|#)
  
  (define (file-mtime path)
    (define who 'file-mtime)
    (let ((msecs
           (mz:with-handlers ((mz:exn:fail:filesystem? (reraise/io-f who path)))
             (mz:file-or-directory-modify-seconds path))))
      (if (exact-non-negative-integer? msecs)
        (* msecs #e1e9)  ;; assumes msecs is in seconds
        (error who "unsupported modification seconds" msecs))))
  
  #;(define (file-ctime path)
    #|Use MzScheme's FFI to use C ctime, I guess|#)
  
  (define file-exists?
    (case-lambda 
      ((path) (file-exists? path #T))
      ((path follow)
       (if follow
         (or (mz:file-exists? path)
             (mz:directory-exists? path))
         (or (mz:link-exists? path)
             (mz:file-exists? path)
             (mz:directory-exists? path))))))
  
  (define file-regular? 
    (case-lambda
      ((path) (file-regular? path #T))
      ((path follow)
       (if follow
         (mz:file-exists? path)
         (and (not (mz:link-exists? path))
              (mz:file-exists? path))))))
  
  (define file-directory?
    (case-lambda 
      ((path) (file-directory? path #T))
      ((path follow)
       (if follow
         (mz:directory-exists? path)
         (and (not (mz:link-exists? path))
              (mz:directory-exists? path))))))
  
  (define (file-symbolic-link? path)
    (mz:link-exists? path))
  
  (define (file-perm? who path perm)
    (and (memq perm (mz:list->mlist
                     (mz:with-handlers ((mz:exn:fail:filesystem? (reraise/io-f who path)))
                       (mz:file-or-directory-permissions path))))
         #T))
  
  (define (file-readable? path)
    (file-perm? 'file-readable? path 'read)) 
  
  (define (file-writable? path)
    (file-perm? 'file-writable? path 'write))
  
  (define (file-executable? path)
    (file-perm? 'file-executable? path 'execute))
  
  (define (file-size path)
    (mz:with-handlers ((mz:exn:fail:filesystem? (reraise/io-f 'file-size path)))
      (mz:file-size path)))
  
  (define rename-file 
    (case-lambda
      ((old new)
       (rename-file old new #F))
      ((old new exists-ok)
       (mz:with-handlers ((mz:exn:fail:filesystem? (reraise/io-f 'rename-file old)))
         (mz:rename-file-or-directory old new exists-ok)))))
)
