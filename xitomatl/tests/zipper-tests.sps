#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (xitomatl zipper trees)
  (srfi :78 lightweight-testing))

(define t0 '(a (b c) d (e (f g) h)))
(define z0 (zip-iterate/df t0))
(check (zip-finish z0) => '(a (b c) d (e (f g) h)))
(check ((zipper-cont z0) "foo") => "foo")
(check (zip-finish ((zipper-cont ((zipper-cont z0) :zip-keep-val:)) 'A)) 
       => '(A (b c) d (e (f g) h)))
(check (zip-finish ((zipper-cont (zip-to-nth/df t0 8)) 'asdf))
       => '(a (b c) d (e asdf h)))

(check (zip-finish z0) => '(a (b c) d (e (f g) h)))

(check-report)
