#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(import
  (rnrs)
  (xitomatl queue)
  (srfi :78 lightweight-testing))

(define q (make-empty-queue))
(check-queue q)
(check (queue-length q) => 0)

(enqueue! q 'asdf)
(check-queue q)
(check (queue-length q) => 1)

(enqueue! q "blah")
(check-queue q)
(check (queue-length q) => 2)

(enqueue! q '(3 2 1))
(check-queue q)
(check (queue-length q) => 3)

(enqueue! q '#(#\a #\b #\c #\d))        
(check-queue q)
(check (queue-length q) => 4)

(check (dequeue! q) => 'asdf)
(check-queue q)
(check (queue-length q) => 3)

(check (dequeue! q) => "blah")
(check-queue q)
(check (queue-length q) => 2)

(check (dequeue! q) => '(3 2 1))
(check-queue q)
(check (queue-length q) => 1)

(check (dequeue! q) => '#(#\a #\b #\c #\d))
(check-queue q)
(check (queue-length q) => 0)

(enqueue! q 'again)
(check-queue q)
(check (queue-length q) => 1)

(check (dequeue! q) => 'again)
(check-queue q)
(check (queue-length q) => 0)

(enqueue! q 'again-again)
(check-queue q)
(check (queue-length q) => 1)

(enqueue! q "2nd")
(check-queue q)
(check (queue-length q) => 2)

(check (dequeue! q) => 'again-again)
(check-queue q)
(check (queue-length q) => 1)

(check (dequeue! q) => "2nd")
(check-queue q)
(check (queue-length q) => 0)

(check-report)
