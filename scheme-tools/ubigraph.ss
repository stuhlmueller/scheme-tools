#!r6rs

(library

 (scheme-tools ubigraph)

 (export ubi-node
         ubi-edge
         ubi-id-edge
         ubi-edge-attrib
         ubi-id-edge-attrib
         ubi-node-attrib
         ubi-reset)

 (import (rnrs)
         (scheme-tools external)
         (scheme-tools py-pickle))

 (define ubigraph-port #f)

 (define (ubigraph . args)
   (py-pickle ubigraph-port args))

 (define (ubi-node id label)
   (ubigraph 'node id label))

 (define (ubi-edge id1 id2 label)
   (ubigraph 'edge id1 id2 label))

 (define (ubi-id-edge edgeid id1 id2 label)
   (ubigraph 'edge edgeid id1 id2 label))

 (define (ubi-edge-attrib attrib val)
   (ubigraph 'edge_attrib attrib val))

 (define (ubi-id-edge-attrib edgeid attrib val)
   (ubigraph 'edge_attrib edgeid attrib val))

 (define (ubi-node-attrib id attrib val)
   (ubigraph 'node_attrib id attrib val))

 (define (ubi-reset)
   (when (not ubigraph-port)
         (set! ubigraph-port (open-py-port "ubigraph")))
   (ubigraph 'reset))

 )