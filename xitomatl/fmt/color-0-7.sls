#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl fmt color (0 7))
  (export
    fmt-in-html fmt-color fmt-in-html? fmt-use-html-font?
    fmt-colored fmt-red fmt-blue fmt-green fmt-cyan fmt-yellow
    fmt-magenta fmt-black fmt-white fmt-bold fmt-underline)
  (import
    (except (rnrs) bitwise-ior bitwise-and)
    (xitomatl include)
    (xitomatl fmt base (0 7))
    (xitomatl fmt srfi-33))

  (include/resolve ("xitomatl" "fmt") "fmt-color.scm")
)
