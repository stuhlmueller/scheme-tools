#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (srfi :78 lightweight-testing)
  (xitomatl indexes))

(check (iota 0) => '())
(check (iota 1) => '(0))
(check (iota 2) => '(0 1))
(check (iota 10) => '(0 1 2 3 4 5 6 7 8 9))
(check (enumerate '()) => '())
(check (enumerate '(a)) => '(0))
(check (enumerate '(a b)) => '(0 1))
(check (enumerate '(a b c d e f g h i)) => '(0 1 2 3 4 5 6 7 8))
(check (enumerate '#()) => '())
(check (enumerate '#(a)) => '(0))
(check (enumerate '#(a b)) => '(0 1))
(check (enumerate '#(a b c d e f g h i)) => '(0 1 2 3 4 5 6 7 8))
(check (enumerate "") => '())
(check (enumerate "a") => '(0))
(check (enumerate "ab") => '(0 1))
(check (enumerate "abcdefghi") => '(0 1 2 3 4 5 6 7 8))
(check (enumerate '#vu8()) => '())
(check (enumerate '#vu8(255)) => '(0))
(check (enumerate '#vu8(255 255)) => '(0 1))
(check (enumerate '#vu8(255 255 255 255 255 255 255 255 255)) => '(0 1 2 3 4 5 6 7 8))


(check-report)
