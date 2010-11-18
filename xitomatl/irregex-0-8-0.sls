#!r6rs
;; Copyright 2010 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl irregex (0 8 0))
  (export
    irregex string->irregex sre->irregex irregex?

    irregex-new-matches irregex-reset-matches! irregex-match-data?
    make-irregex-match irregex-match-chunker-set!
    irregex-match-start-chunk-set! irregex-match-start-index-set!
    irregex-match-end-chunk-set! irregex-match-end-index-set!
    irregex-match-valid-index? irregex-match-index
    irregex-match-num-submatches irregex-match-substring
    irregex-match-start-chunk irregex-match-start-index
    irregex-match-end-chunk irregex-match-end-index
    irregex-match-subchunk irregex-match-chunker

    irregex-search irregex-search/matches irregex-match irregex-match?
    irregex-replace irregex-replace/all
    irregex-search/chunked irregex-match/chunked
    irregex-fold irregex-fold/chunked irregex-fold/fast irregex-fold/chunked/fast
    irregex-extract irregex-split

    make-irregex-chunker chunker-get-next chunker-get-str chunker-get-start
    chunker-get-end chunker-get-substring chunker-get-subchunk

    irregex-dfa irregex-dfa/search irregex-dfa/extract
    irregex-nfa irregex-flags irregex-num-submatches irregex-lengths irregex-names
    irregex-quote irregex-opt sre->string string->sre maybe-string->sre
    string-cat-reverse)
  (import
    (rename (except (rnrs) error remove)
            (exists any) (for-all every) (remp remove))
    (rnrs mutable-strings)
    (rnrs mutable-pairs)
    (rnrs r5rs)
    (srfi :6 basic-string-ports)
    (srfi :23 error tricks)
    (only (xitomatl include) include/resolve))

  (SRFI-23-error->R6RS "(library (xitomatl irregex (0 8 0)))"
   (include/resolve ("xitomatl" "irregex") "irregex-r6rs.scm")
   (include/resolve ("xitomatl" "irregex") "irregex-utils.scm"))
)
