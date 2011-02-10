#!r6rs

;; A wrapper around scsh-components that operates on the (scheme-tools
;; graph) data structure.

;; The scsh module assumes that the links are not labelled and that
;; there is only one link between two nodes in a graph. Since weights
;; and multiple links don't change the structure of the components
;; that are found, we can simply reduce the scheme-tools graph to a
;; scsh graph.

(library

 (scheme-tools graph components)

 (export strongly-connected-components)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools srfi-compat :1)
         (scheme-tools graph)
         (scheme-tools graph scsh-components))

 (define (graph->scsh-graph graph)
   (map (lambda (node&links)
          (let ([from (first node&links)]
                [links (rest node&links)])
            (pair from (map link->target links))))
        (graph->alist graph)))

 (define (strongly-connected-components graph)
   (let ([scsh-graph (graph->scsh-graph graph)])
     (scsh-strongly-connected-components scsh-graph)))
 
 )