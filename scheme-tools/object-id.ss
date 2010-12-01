#!r6rs

;; Compute unique IDs for objects hashable by (scheme-tools hash).

;; WARNING:
;; This throws out procedures. We assume that any object given fully
;; mirrors procedure information in an accessible way.

(library

 (scheme-tools object-id)

 (export object->id
         id->object
         reset-object-ids!)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools hash))

 (define obj-id-maker (symbol-maker 'obj))
 
 (define obj->id-table (make-hash-table))
 
 (define id->obj-table (make-hash-table))

 (define (reset-object-ids!)
   (set! obj-id-maker (symbol-maker 'obj))
   (set! obj->id-table (make-hash-table))
   (set! id->obj-table (make-hash-table)))

 (define (make-object-id-entries obj id)
   (hash-table-set! obj->id-table
                    obj
                    id)
   (hash-table-set! id->obj-table
                    id
                    obj))

 (define (object->id obj)
   (hash-table-ref obj->id-table
                   obj
                   (lambda ()
                     (let ([id (obj-id-maker)])
                       (make-object-id-entries obj id)
                       id))))

 (define (id->object id)
   (hash-table-ref id->obj-table
                   id
                   (lambda () (error id "no object found!"))))

 )