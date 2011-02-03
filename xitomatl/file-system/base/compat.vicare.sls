;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl file-system base compat)
  (export
    directory-enumerator directory-list
    current-directory delete-directory delete-file
    make-directory make-symbolic-link change-mode file-mtime file-ctime
    file-exists? file-regular? file-directory? file-symbolic-link?
    file-readable? file-writable? file-executable? file-size rename-file)
  (import
    (rnrs)
    (only (ikarus) current-directory delete-directory delete-file
                   make-directory make-symbolic-link change-mode file-mtime file-ctime
                   file-exists? file-regular? file-directory? file-symbolic-link?
                   file-readable? file-writable? file-executable? file-size)
    (only (ikarus) open-directory-stream read-directory-stream close-directory-stream)
    (prefix (only (ikarus) rename-file) ik:)
    (only (xitomatl common) format))

  (define (directory-enumerator/who who path proc seeds)
    ;; NOTE: Ikarus uses its guardians to close directory-streams which didn't
    ;; get closed before they're GC'd, which can happen if an exception is
    ;; raised in this procedure.
    (define (ignore? e) (or (string=? e ".") (string=? e "..")))
    (define (done rets)
      (close-directory-stream ds)
      (apply values rets))
    (define ds
      (with-exception-handler
        (lambda (ex)
          (raise
            (condition (make-who-condition who)
                       (if (condition? ex)
                         ex
                         (make-irritants-condition (list ex))))))
        (lambda () (open-directory-stream path))))
    (let loop ((seeds seeds))
      (let ((e (read-directory-stream ds)))
        (if e
          (if (ignore? e)
            (loop seeds)
            (let-values (((cont . next-seeds) (apply proc e seeds)))
              (if cont
                (loop next-seeds)
                (done next-seeds))))
          (done seeds)))))

  (define (directory-enumerator path proc seeds)
    (directory-enumerator/who 'directory-enumerator path proc seeds))
   
  (define (directory-list path)
    (directory-enumerator/who 'directory-list
     path
     (lambda (e a) (values #T (cons e a)))
     '(())))
 
  (define rename-file 
    (case-lambda
      ((old new)
       (rename-file old new #F))
      ((old new exists-ok)
       (when (and (not exists-ok) (file-exists? new #F))
         (raise (condition (make-who-condition 'rename-file)
                           (make-message-condition 
                            (format "already exists: ~a" new))
                           (make-i/o-filename-error old))))
       (ik:rename-file old new))))
)
