#!r6rs
(library (xitomatl stack-lang unsafe)
  (export
    $car $cdr)
  (import
    (prefix (only (rnrs base) car cdr) $))
)
