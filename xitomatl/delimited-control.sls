#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

;; Taken from Oleg's http://okmij.org/ftp/Scheme/delim-control-n.scm
;; NOTE: Not currently designed for multi-threaded use.
;;       Won't work across phases on a multiple-instantiation system.

(library (xitomatl delimited-control)
  (export 
    abort prompt control shift reset prompt0 control0 shift0 reset0)
  (import 
    (rnrs))
  
  (define cells '())
  (define (cell-push! x) (set! cells (cons x cells)))
  (define (cell-pop!) 
    (let ((x (car cells))) 
      (set! cells (cdr cells)) 
      x))

  (define-record-type cell (fields cont (mutable mark)))
  
  ; Essentially this is the ``return from the function''
  (define (abort-top! v) ((cell-cont (cell-pop!)) v))
  
  (define (unwind-till-marked! keep? accum)
    (let ((c (if (null? cells)
               (error 'unwind-till-marked! "no prompt set")
               (car cells))))  ; peek at the top cell
      (if (cell-mark c)	; if marked, it's prompt's cell
        (begin (unless keep? (cell-mark-set! c #F))
               accum)
        (begin (set! cells (cdr cells)) ; remove cell from the top of stack 
               (unwind-till-marked! keep? (cons c accum))))))
  
  (define (make-control shift? keep?)
    (lambda (f)
      (call/cc
        (lambda (k-control)
          (let* ((cells-prefix (unwind-till-marked! keep? '()))
                 (invoke-subcont (lambda (v)
                                   (call/cc
                                     (lambda (k-return)
                                       (cell-push! (make-cell k-return shift?))
                                       (for-each cell-push! cells-prefix)
                                       (k-control v))))))
            (abort-top! (f invoke-subcont)))))))
  
  (define (prompt* thunk)
    (call/cc
      (lambda (outer-k)
        (cell-push! (make-cell outer-k #T)) ; it's prompt's cell
        (abort-top! (thunk)))))
  
  (define control* (make-control #F #T))
  
  (define (abort v) (control* (lambda (ignore) v)))
  
  (define-syntax prompt
    (syntax-rules ()
      ((_ e) (prompt* (lambda () e)))))
  
  (define-syntax control
    (syntax-rules ()
      ((_ k e) (control* (lambda (k) e)))))
  
  (define-syntax reset
    (syntax-rules ()
      ((_ e) (prompt e))))  
  
  (define shift* (make-control #T #T))
  
  (define-syntax shift
    (syntax-rules ()
      ((_ k e) (shift* (lambda (k) e)))))
  
  (define-syntax prompt0
    (syntax-rules ()
      ((_ e) (prompt e))))
  
  (define control0* (make-control #F #F))
  
  (define-syntax control0
    (syntax-rules ()
      ((_ k e) (control0* (lambda (k) e)))))
  
  (define-syntax reset0
    (syntax-rules ()
      ((_ e) (prompt e))))  
  
  (define shift0* (make-control #T #F))
  
  (define-syntax shift0
    (syntax-rules ()
      ((_ k e) (shift0* (lambda (k) e)))))
)
