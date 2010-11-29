#!r6rs

(library

 (scheme-tools queue)

 (export queue?
         make-empty-queue
         enqueue!
         dequeue!
         queue-length
         queue-empty?
         queue->list
         queue->list/reset
         check-queue
         in-queue?
         enqueue-if-new!)

 (import (xitomatl queue)
         (xitomatl curry)
         (rnrs)
         (only (_srfi :1) find))

 (define/curry (in-queue? eql q obj)
   (if (find (lambda (x) (eql obj x)) (queue->list q)) #t #f))

 (define/curry (enqueue-if-new! eql q obj)
   (when (not (in-queue? eql q obj))
         (enqueue! q obj)))

 )