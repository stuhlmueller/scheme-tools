#!r6rs

(library

 (scheme-tools graph utils)

 (export traverse
         graph:add/retrieve!
         graph:add/link!)

 (import (rnrs)
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

 )