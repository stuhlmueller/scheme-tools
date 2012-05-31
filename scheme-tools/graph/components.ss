#!r6rs

;; A wrapper around scsh-components that operates on the (scheme-tools
;; graph) data structure.

;; The scsh module assumes that the links are not labelled and that
;; there is only one link between two nodes in a graph. Since weights
;; and multiple links don't change the structure of the components
;; that are found, we can simply reduce the scheme-tools graph to a
;; scsh graph.

;; Returns components in topological order (i.e. if there is a link
;; from component A to component B, A comes first).

(library

 (scheme-tools graph components)

 (export strongly-connected-components)

 (import (rnrs)
         (scheme-tools external)
         (scheme-tools readable-scheme)
         (scheme-tools srfi-compat :1)
         (scheme-tools object-id)
         (scheme-tools graph)
         (scheme-tools graph scsh-components))

 (define (graph->scsh-graph graph)
   (map (lambda (node&links)
          (let ([from (first node&links)]
                [links (rest node&links)])
            (pair (object->id from)
                  (map ($ object->id link->target) links))))
        (graph->alist graph)))

 (define (strongly-connected-components graph)
   (let* ([scsh-graph (graph->scsh-graph graph)]
          [scsh-components (scsh-strongly-connected-components scsh-graph)])
     (reverse (map (lambda (component) (map id->object component))
                   scsh-components))))

 )