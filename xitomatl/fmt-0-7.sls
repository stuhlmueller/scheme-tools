#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl fmt (0 7))
  (export
    new-fmt-state
    fmt fmt-start fmt-if fmt-capture fmt-let fmt-bind fmt-null
    fmt-ref fmt-set! fmt-add-properties! fmt-set-property!
    fmt-col fmt-set-col! fmt-row fmt-set-row!
    fmt-radix fmt-set-radix! fmt-precision fmt-set-precision!
    fmt-properties fmt-set-properties! fmt-width fmt-set-width!
    fmt-writer fmt-set-writer! fmt-port fmt-set-port!
    fmt-decimal-sep fmt-set-decimal-sep!
    copy-fmt-state
    fmt-file fmt-try-fit cat apply-cat nl fl nl-str
    fmt-join fmt-join/last fmt-join/dot
    fmt-join/prefix fmt-join/suffix fmt-join/range
    pad pad/right pad/left pad/both trim trim/left trim/both trim/length
    fit fit/left fit/both tab-to space-to wrt wrt/unshared dsp
    pretty pretty/unshared slashified maybe-slashified
    num num/si num/fit num/comma num/roman num/old-roman
    radix fix decimal-align ellipses
    upcase downcase titlecase pad-char comma-char decimal-char
    with-width wrap-lines fold-lines justify
    make-string-fmt-transformer
    make-space make-nl-space display-to-string write-to-string
    fmt-columns columnar line-numbers)
  (import
    (xitomatl fmt base (0 7))
    (xitomatl fmt pretty (0 7))
    (xitomatl fmt column (0 7)))
)
