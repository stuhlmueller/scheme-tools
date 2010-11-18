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
    (prefix (primitives gensym) larceny:)
    (primitives pretty-print
                time
                with-input-from-string
                with-output-to-string
                system)
    ;; Can't use Larceny's format because some of its directives differ.
    (prefix (srfi :48 intermediate-format-strings) IFS:))

  (define (add1 x) (+ 1 x))

  (define (sub1 x) (- x 1))

  (define (format fmt-str . fmt-args)
    (apply IFS:format #F fmt-str fmt-args))
  
  (define (printf fmt-str . fmt-args)
    (apply IFS:format #T fmt-str fmt-args))
  
  (define (fprintf port fmt-str . fmt-args)
    (unless (and (output-port? port) (textual-port? port))
      (assertion-violation 'fprintf "not a textual output port" port))
    (apply IFS:format port fmt-str fmt-args))

  (define gensym
    (case-lambda
      (()
       (larceny:gensym "gensym"))
      ((name)
       (larceny:gensym (cond ((string? name) name)
                             ((symbol? name) (symbol->string name))
                             (else (assertion-violation 'gensym
                                    "not a string or symbol" name)))))))
)
