#!r6rs

(library

 (scheme-tools graph)

 (export make-graph
         graph->alist
         display-graph
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
         graph:leaf?
         graph:root?
         link->label
         link->weight
         link->target)

 (import (scheme-tools graph graph))

 )