#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl queue)  
  (export 
    queue?
    make-empty-queue
    enqueue!
    dequeue!
    queue-length
    queue-empty?
    queue->list
    queue->list/reset
    check-queue)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (xitomatl define) define/AV))
  
  (define-record-type queue 
    (fields (mutable head) (mutable end)))
  
  (define (make-empty-queue)
    (make-queue '() '()))
  
  (define (enqueue! q e)
    (let ((el (cons e '())))
      (let ((qe (queue-end q)))
        (unless (null? qe) (set-cdr! qe el))
        (queue-end-set! q el)
        (when (null? (queue-head q)) (queue-head-set! q el)))))
  
  (define/AV (dequeue! q)
    (let ((h (queue-head q)))
      (when (null? h) (AV "empty queue"))
      (let ((rest (cdr h)))
        (queue-head-set! q rest)
        (when (null? rest) (queue-end-set! q '())))
      (car h)))
  
  (define (queue-length q)
    (length (queue-head q)))
  
  (define (queue-empty? q)
    (= 0 (queue-length q)))
  
  (define (queue->list q)
    (apply list (queue-head q)))
  
  (define (queue->list/reset q)
    (let ((h (queue-head q)))
      (queue-head-set! q '())
      (queue-end-set! q '())
      h))
  
  (define/AV (check-queue q)
    (let ((head (queue-head q))
          (end (queue-end q)))
      (if (null? head)
        (unless (null? end)
          (AV "head is null but end is not"))
        (unless (eq? end (list-tail head (- (length head) 1)))
          (AV "last pair of head is not end")))
      (if (null? end)
        (unless (null? head)
          (AV "end is null but head is not"))
        ; We know head is not null, therefore previous if block 
        ; will have checked that last pair of head is end
        )))
)
