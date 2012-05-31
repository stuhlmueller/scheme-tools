#!r6rs

(library

 (scheme-tools graph callback)

 (export graph:ancestor-callbacks
         graph:callbacks
         graph:register-callback!)

 (import (rnrs)
         (scheme-tools readable-scheme)
         (scheme-tools hash)
         (scheme-tools property)
         (scheme-tools watcher)
         (scheme-tools graph graph)
         (scheme-tools graph utils))

 (define (graph->callback-registry graph)
   (get/set-property graph
                     'callback-registry
                     make-finitize-hash-table))

 (define (graph:register-callback! graph node callback)
   (hash-table-set! (graph->callback-registry graph)
                    node
                    (pair callback (graph:callbacks graph node))))

 (define (graph:callbacks graph node)
   (hash-table-ref/default (graph->callback-registry graph)
                           node
                           '()))

 (define (graph:ancestor-callbacks graph node)
   (traverse node
             (lambda (node) (graph:parents graph node))
             (lambda (node list-of-callback-lists)
               (append (graph:callbacks graph node)
                       (apply append list-of-callback-lists)))
             (make-watcher)
             '()))

 )