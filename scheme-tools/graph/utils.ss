#!r6rs

(library

 (scheme-tools graph utils)

 (export traverse
         graph:add/retrieve!
         graph:add/link!
         graph:root-nodes
         display-graph)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools object-id)
         (scheme-tools repl)
         (scheme-tools srfi-compat :1)
         (scheme-tools graph graph))

 (define (traverse start next combine stop? default)
   (if (stop? start)
       default
       (combine start
                (map (lambda (obj) (traverse obj next combine stop? default))
                     (next start)))))

 (define (graph:add/retrieve! graph node)
   (let* ([is-new (not (graph:node-exists? graph node))])
     (when is-new
           (graph:add-node! graph node))
     is-new))

 (define (graph:add/link! graph last-node node label weight)
   (let* ([is-new (not (graph:node-exists? graph node))]
          [connect! (if is-new graph:add-child! graph:link!)])
     (connect! graph last-node node label weight)
     is-new))

 (define (graph:root-nodes graph)
   (filter (lambda (node) (null? (graph:parents graph node)))
           (map first (graph->alist graph))))

 (define (display-node node links node->info)
   (pe "node: " (if node->info (node->info node) node) "\n")
   (if links
       (begin
         (for-each (lambda (link) (pe "  " link "\n"))
                   links)
         (pe "\n"))))

 (define (display-graph graph . node->info)
   (for-each (lambda (entry) (display-node (first entry) (rest entry) (if (null? node->info) #f (first node->info))))
             (graph->alist graph)))

 )