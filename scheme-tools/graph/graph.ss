#!r6rs

;; Library for graphs with weighted, labelled links and
;; bidirectional access (children->parents, parents->children)

;; WARNING:
;; This throws out procedures in its equality check of new nodes
;; against old nodes. We assume that any object given fully mirrors
;; procedure information in an accessible way.

(library

 (scheme-tools graph graph)

 (export graph?
         make-graph
         graph->alist
         graph-size
         graph:root
         graph:set-root!
         graph:add-node!
         graph:children
         graph:child-links
         graph:parents         
         graph:parent-links
         graph:link!
         graph:add-child!
         graph:node-exists?
         graph:remove-node!
         graph:leaf?
         graph:root?
         make-link
         link->label
         link->weight
         link->target)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (scheme-tools hash)
         (scheme-tools external)
         (scheme-tools readable-scheme))

 (define-record-type graph
   (fields (mutable root graph:root graph:set-root!)
           (mutable table graph:table graph:set-table!)
           (mutable uptable graph:uptable graph:set-uptable!))
   (protocol
    (lambda (p)
      (lambda () (p 'empty (make-finitize-hash-table) (make-finitize-hash-table))))))

 (define (make-link label weight target)
   (pair label (pair weight target)))

 (define link->label first)

 (define link->weight second)

 (define link->target cddr)

 (define (graph->alist graph)
   (hash-table->alist (graph:table graph)))

 (define (graph-size graph)
   (length (graph->alist graph)))

 (define (graph:node-exists? graph node)
   (hash-table-ref/default (graph:table graph) node #f))

 (define (graph:add-node! graph node)
   (when (not (graph:node-exists? graph node))
         (hash-table-set! (graph:table graph) node '())
         (hash-table-set! (graph:uptable graph) node '())))

 (define (graph:children graph node)
   (map link->target (graph:child-links graph node)))

 (define (graph:child-links graph node)
   (hash-table-ref/default (graph:table graph) node '()))

 (define (graph:parents graph node)
   (map link->target (graph:parent-links graph node)))

 (define (graph:parent-links graph node)
   (hash-table-ref/default (graph:uptable graph) node '()))

 (define (list-replace test replacer lst)
   (if (null? lst)
       '()
       (pair (let ([init (first lst)])
               (if (test init)
                   (replacer init)
                   init))
             (list-replace test
                           replacer
                           (rest lst)))))

 ;; find old link using label
 (define (table:add-child! table parent child label weight . allow-update)
   (let* ([links (hash-table-ref table parent)]
          [old-link (assoc label links)]
          [new-link (pair label (pair weight child))])
     (if (false? old-link)
         (hash-table-set! table
                          parent
                          (pair new-link links))
         (if (null? allow-update)
             (assert (finitize-equal? old-link new-link))
             (let ([new-links (list-replace (lambda (link) (equal? (link->label link) label))
                                            (lambda (link) new-link)
                                            links)])
               (hash-table-set! table
                                parent
                                new-links))))))

 ;; find old link using source/target
 (define (table:add-parent! table parent child label weight . allow-update)
   (let* ([links (hash-table-ref table child)]
          [link-identifier (lambda (link) (and (finitize-equal? (link->target link) parent)
                                          (equal? (link->label link) label)))]
          [old-link (find link-identifier links)]
          [new-link (pair label (pair weight parent))])
     (if (false? old-link)
         (hash-table-set! table
                          child
                          (pair new-link links))
         (if (null? allow-update)
             (assert (finitize-equal? old-link new-link))
             (let ([new-links (list-replace link-identifier
                                            (lambda (link) new-link)
                                            links)])
               (hash-table-set! table
                                child
                                new-links))))))

 ;; child - v, p -> parent | label not unique, probabilities don't sum to 1
 ;; parent - v, p -> child | label unique, probabilities sum to 1
 ;; allow-update can only update weights reliably
 (define (graph:link! graph parent child label weight . allow-update)
   (apply table:add-child! (append (list (graph:table graph) parent child label weight) allow-update))
   (apply table:add-parent! (append (list(graph:uptable graph) parent child label weight) allow-update)))

 (define (graph:add-child! graph node child label weight)
   (graph:add-node! graph child)
   (graph:link! graph node child label weight))

 (define (table:remove-link! table from to)
   (let* ([links (hash-table-ref table from)])
     (hash-table-set! table
                      from
                      (filter (lambda (link) (not (finitize-equal? (link->target link) to)))
                              links))))

 ;; get children from table
 ;; for each child, remove node as a parent in uptable
 ;; get parents from uptable
 ;; for each parent, remove node as child in table
 ;; remove node from table and uptable
 (define (graph:remove-node! graph node)
   (let ([children (graph:children graph node)]
         [parents (graph:parents graph node)])
     (for-each (lambda (child) (table:remove-link! (graph:uptable graph) child node))
               children)
     (for-each (lambda (parent) (table:remove-link! (graph:table graph) parent node))
               parents)
     (hash-table-delete! (graph:table graph) node)
     (hash-table-delete! (graph:uptable graph) node)))

 (define (graph:leaf? graph node)
   (null? (graph:children graph node)))

 (define (graph:root? graph node)
   (finitize-equal? node (graph:root graph)))

 )