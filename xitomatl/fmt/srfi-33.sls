#!r6rs
(library (xitomatl fmt srfi-33)
  (export
    arithmetic-shift
    bitwise-and
    bitwise-ior)
  (import
    (rnrs base)
    (only (rnrs r5rs) quotient)
    (xitomatl include))

  (include/resolve ("xitomatl" "fmt") "srfi-33.scm")
)
