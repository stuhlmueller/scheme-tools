;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (only (xitomatl common) printf)
  (only (xitomatl file-system base)
        directory-walk-enumerator make-path-to rename-file)
  (xitomatl file-system paths)
  (only (xitomatl enumerators) fold/enumerator)
  (xitomatl match))

(define (move-out base temp)
  (define moves
    (fold/enumerator
     (directory-walk-enumerator)
     base
     (lambda (path dirs files syms accum)
       (let loop
           ((others-specific
             (filter
              (matches? (:regex '(seq (+ any) "." (submatch (+ alpha)) ".sls")
                                (:not "larceny")))
              (append files syms)))
            (accum accum))
         (if (null? others-specific)
           (values dirs accum)
           (let* ((orig (path-join path (car others-specific)))
                  (reloc (path-join temp orig)))
             (loop (cdr others-specific)
                   (cons (cons orig reloc) accum))))))
     '()))
  (display "Temporarily relocating other implementations' specific files ...\n")
  (for-each
   (lambda (m)
     (let ((from (car m)) (to (cdr m)))
       (printf "~a\n--> ~a\n" from to)
       (make-path-to to)
       (rename-file from to)))
   moves)
  moves)

(define (move-back moves)
  (display "Moving relocated files back ...\n")
  (for-each
   (lambda (m)
     (let ((to (car m)) (from (cdr m)))
       (printf "~a\n--> ~a\n" from to)
       (rename-file from to)))
   moves))

(define (compile)
  ;; TODO: Use Larceny facilities for compiling.
  (display "\nNow, manually make Larceny compile the library files left in the tree.\n")
  (display "When done, type ENTER to move relocated files back.\n\n")
  (get-line (current-input-port)))

(define main
  (match-lambda*
    ((base temp)
     (and (path? base) (path? temp))
     (let ((moves (move-out base temp)))
       (compile)
       (move-back moves)))
    (args
     (apply assertion-violation (car (command-line))
            "invalid command-line arguments" args))))

(apply main (cdr (command-line)))
