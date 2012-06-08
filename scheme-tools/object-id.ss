#!r6rs

;; Compute unique IDs for objects hashable by (scheme-tools hash).

;; WARNING:
;; This identifies all procedures for the purpose of hashing. We
;; assume that any object given fully mirrors procedure information in
;; an accessible way.

(library

 (scheme-tools object-id)

 (export object->id
         id->object
         reset-object-ids!)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools hash))

 (define obj-id-maker (get-counter))

 (define obj->id-table (make-finitize-hash-table))

 (define id->obj-table (make-eq-hash-table))

 (define (reset-object-ids!)
   (set! obj-id-maker (get-counter))
   (set! obj->id-table (make-finitize-hash-table))
   (set! id->obj-table (make-eq-hash-table)))

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