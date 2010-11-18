#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl fmt base (0 7))
  (export
    call-with-output-string
    make-eq?-table
    write-to-string
    display-to-string
    nl-str
    make-space
    make-nl-space
    take*
    drop*
    *default-fmt-state*
    fmt-state?
    new-fmt-state
    copy-fmt-state
    fmt-row
    fmt-col
    fmt-radix
    fmt-properties
    fmt-pad-char
    fmt-precision
    fmt-width
    fmt-writer
    fmt-port
    fmt-decimal-sep
    fmt-decimal-align
    fmt-string-width
    fmt-ellipses
    fmt-set-row!
    fmt-set-col!
    fmt-set-radix!
    fmt-set-properties!
    fmt-set-pad-char!
    fmt-set-precision!
    fmt-set-width!
    fmt-set-writer!
    fmt-set-port!
    fmt-set-decimal-sep!
    fmt-set-decimal-align!
    fmt-set-string-width!
    fmt-set-ellipses!
    fmt-ref
    fmt-set-property!
    fmt-set!
    fmt-add-properties!
    fmt-let
    fmt-bind
    fix
    radix
    pad-char
    comma-char
    decimal-char
    decimal-align
    with-width
    ellipses
    fmt-start
    fmt
    fmt-update
    fmt-write
    apply-cat
    cat
    fmt-null
    fmt-if
    fmt-try-fit
    fits-in-width
    fits-in-columns
    fmt-capture
    fmt-to-string
    nl
    fl
    tab-to
    space-to
    fmt-join
    fmt-join/prefix
    fmt-join/suffix
    fmt-join/last
    fmt-join/dot
    fmt-join/range
    pad/both
    pad
    pad/right
    pad/left
    trim/buffered
    trim
    trim/length
    trim/left
    trim/both
    fit
    fit/left
    fit/both
    make-string-fmt-transformer
    upcase
    downcase
    titlecase
    *min-e*
    *bot-f*
    integer-log
    integer-length*
    invlog2of
    fast-expt
    mirror-of
    default-digits
    num->string
    num
    num/comma
    num/si
    roman-numerals
    num/old-roman
    num/roman
    num/fit
    eq?-table-ref
    eq?-table-set!
    make-shared-ref-table
    gen-shared-ref
    maybe-gen-shared-ref
    call-with-shared-ref
    call-with-shared-ref/cdr
    slashified
    maybe-slashified
    fmt-write-string
    dsp
    write-with-shares
    wrt
    wrt/unshared)
  (import
    (rename (rnrs)
            (call-with-string-output-port call-with-output-string))
    (rnrs mutable-pairs)
    (only (rnrs r5rs)
          exact->inexact inexact->exact modulo quotient remainder)
    (only (srfi :1 lists)
          make-list)
    (srfi :6 basic-string-ports)
    (only (srfi :13 strings)
          substring/shared string-index string-index-right
          string-count string-concatenate-reverse reverse-list->string)
    (srfi :23 error tricks)
    (only (srfi :69 basic-hash-tables)
          make-hash-table hash-table-ref/default
          hash-table-set! hash-table-walk)
    (xitomatl include)
    (xitomatl fmt let-optionals*))

  (define (make-eq?-table) (make-hash-table eq?))

  (define (mantissa+exponent num . opt)
    ;; Break a positive real number down to a normalized mantissa and
    ;; exponent. Default base=2, mant-size=52, exp-size=11 for IEEE doubles.
    (if (zero? num)
      (list 0 0)
      (let-optionals* opt ((base 2) (mant-size 52) (exp-size 11))
        (let* ((bot (expt base mant-size))
               (top (* base bot)))
          (let lp ((n num) (e 0))
            (cond
              ((>= n top) (lp (quotient n base) (+ e 1)))
              ((< n bot) (lp (* n base) (- e 1)))
              (else (list n e))))))))

  (SRFI-23-error->R6RS "(library (xitomatl fmt base (0 7)))"
   (include/resolve ("xitomatl" "fmt") "fmt.scm"))
)
