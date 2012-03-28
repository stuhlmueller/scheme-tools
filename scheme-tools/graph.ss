#!r6rs

(library

 (scheme-tools graph)

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

 (import (scheme-tools graph graph))

 )