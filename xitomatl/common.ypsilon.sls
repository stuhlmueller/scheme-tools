;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl common)
  (export
    add1 sub1
    format printf fprintf pretty-print
    gensym
    time
    with-input-from-string with-output-to-string
    system
    ;; TODO: add to as needed/appropriate
    )
  (import
    (rnrs)
    (only (core)
          format system
          set-current-input-port! set-current-output-port!)
    (prefix (only (core) pretty-print gensym) ypsilon:)
    (only (time) time))
  
  (define (add1 x) (+ x 1))

  (define (sub1 x) (- x 1))
  
  (define (fprintf port fmt-str . fmt-args)
    (put-string port (apply format fmt-str fmt-args)))
  
  (define (printf fmt-str . fmt-args)
    (apply fprintf (current-output-port) fmt-str fmt-args))
  
  (define pretty-print
    (case-lambda
      ((x)
       (pretty-print x (current-output-port)))
      ((x p)
       (ypsilon:pretty-print x p)
       (newline p))))

  (define gensym
    (case-lambda
      (()
       (ypsilon:gensym))
      ((name)
       (ypsilon:gensym (cond ((string? name) name)
                             ((symbol? name) (symbol->string name))
                             (else (assertion-violation 'gensym
                                    "not a string or symbol" name)))))))

  (define (parameterize-current-port port set-port! val thunk)
    (define (swap)
      (let ((t (port)))
        (set-port! val)
        (set! val t)))
    (dynamic-wind swap thunk swap))
  
  (define (with-input-from-string str thunk)
    (parameterize-current-port current-input-port set-current-input-port!
                               (open-string-input-port str) thunk))

  (define (with-output-to-string thunk)
    (let-values (((sop get) (open-string-output-port)))
      (parameterize-current-port current-output-port set-current-output-port!
                                 sop thunk)
      (get)))
)
