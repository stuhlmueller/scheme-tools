#!r6rs

;; Library for graphs with weighted, labelled links and
;; bidirectional access (children->parents, parents->children)

;; WARNING:
;; This throws out procedures in its equality check of new nodes
;; against old nodes. We assume that any object given fully mirrors
;; procedure information in an accessible way.

(library

 (scheme-tools graph)

 (export make-graph
         graph->alist
         display-graph         
         graph:root
         graph:set-root!
         graph:add-node!
         graph:children
         graph:parent-links
         graph:link!
         graph:add-child!
         link->label
         link->weight
         link->target)

 (import (rnrs)
         (_srfi :1)
         (scheme-tools hash)
         (scheme-tools external)
         (scheme-tools readable-scheme))

 (define-record-type graph
   (fields (mutable root graph:root graph:set-root!)
           (mutable table graph:table graph:set-table!)
           (mutable uptable graph:uptable graph:set-uptable!))
   (protocol
    (lambda (p)
      (lambda () (p 'empty (make-hash-table) (make-hash-table) '())))))

 (define link->label first)

 (define link->weight second)

 (define link->target cddr)

 (define (graph->alist graph)
   (hash-table->alist (graph:table graph)))

 (define (display-graph graph)
   (pretty-print (graph->alist graph)))

 (define (graph:add-node! graph node)
   (let ([node-exists (hash-table-ref/default (graph:table graph) node #f)])
     (when (not node-exists)
           (hash-table-set! (graph:table graph) node '())
           (hash-table-set! (graph:uptable graph) node '()))))

 (define (graph:children graph node)
   (let ([links (hash-table-ref/default (graph:table graph) node '())])
     (map link->target links)))

 (define (graph:parent-links graph node)
   (hash-table-ref/default (graph:uptable graph) node '()))

 ;; find old link using label
 (define (table:add-child! table parent child label weight)
   (let* ([links (hash-table-ref table parent)]
          [old-link (assoc label links)]
          [new-link (pair label (pair weight child))])
     (if (false? old-link)
         (hash-table-set! table
                          parent
                          (pair new-link links))
         (assert (requal? old-link new-link)))))

 ;; find old link using source/target
 (define (table:add-parent! table parent child label weight)
   (let* ([links (hash-table-ref table child)]
          [old-link (find (lambda (link) (and (requal? (link->target link) parent)
                                         (equal? (link->label link) label))) links)]
          [new-link (pair label (pair weight parent))])
     (if (false? old-link)
         (hash-table-set! table
                          child
                          (pair new-link links))
         (assert (requal? old-link new-link)))))

 ;; child - v, p -> parent | label not unique, probabilities don't sum to 1
 ;; parent - v, p -> child | label unique, probabilities sum to 1
 (define (graph:link! graph parent child label weight)
   (table:add-child! (graph:table graph) parent child label weight)
   (table:add-parent! (graph:uptable graph) parent child label weight))

 (define (graph:add-child! graph node child label weight)
   (graph:add-node! graph child)
   (graph:link! graph node child label weight))

 )