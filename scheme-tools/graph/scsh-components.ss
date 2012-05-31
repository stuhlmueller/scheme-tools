#!r6rs

;; Based on scsh code.

;; Copyright (c) 1993-2003 Richard Kelsey and Jonathan Rees
;; Copyright (c) 1994-2003 by Olin Shivers and Brian D. Carlstrom.
;; Copyright (c) 1999-2003 by Martin Gasbichler.
;; Copyright (c) 2001-2003 by Michael Sperber.
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the authors may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;; Code to find the strongly connected components of a graph.
;; (TO <vertex>) are the vertices that have an edge to <vertex>.
;; (SLOT <vertex>) and (SET-SLOT! <vertex> <value>) is a settable slot
;; used by the algorithm.
;;
;; The components are returned in a backwards topologically sorted list.

(library

 (scheme-tools graph scsh-components)

 (export scsh-strongly-connected-components)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools srfi-compat :1))

 (define (strongly-connected-components vertices to slot set-slot!)
   (make-vertices vertices to slot set-slot!)
   (let loop ((to-do vertices) (index 0) (stack #t) (comps '()))
     (let ((to-do (find-next-vertex to-do slot)))
       (cond ((null? to-do)
              (for-each (lambda (n) (set-slot! n #f)) vertices)
              comps)
             (else
              (call-with-values
                  (lambda ()
                    (do-vertex (slot (car to-do)) index stack comps))
                (lambda (index stack comps)
                  (loop to-do index stack comps))))))))

 (define (find-next-vertex vertices slot)
   (do ((vertices vertices (cdr vertices)))
       ((or (null? vertices)
            (= 0 (vertex-index (slot (car vertices)))))
        vertices)))

 (define-record-type vertex
   (fields (mutable data)     ; user's data
           (mutable edges)    ; list of vertices
           (mutable stack)    ; next vertex on the stack
           (mutable index)    ; time at which this vertex was reached in the traversal
           (mutable parent)   ; a vertex pointing to this one
           (mutable lowpoint) ; lowest index in this vertices strongly connected component
           )
   (protocol
    (lambda (p)
      (lambda (data) (p data '() #f 0 #f #f)))))

 (define (make-vertices vertices to slot set-slot!)
   (let ((maybe-slot (lambda (n)
                       (let ((s (slot n)))
                         (if (vertex? s)
                             s
                             (error "graph edge points to non-vertex" n))))))
     (for-each (lambda (n)
                 (set-slot! n (make-vertex n)))
               vertices)
     (for-each (lambda (n)
                 (vertex-edges-set! (slot n) (map maybe-slot (to n))))
               vertices)
     (values)))

 ;; The numbers are the algorithm step numbers from page 65 of Graph Algorithms,
 ;; Shimon Even, Computer Science Press, 1979.

 ;; 2

 (define (do-vertex vertex index stack comps)
   (let ((index (+ index '1)))
     (vertex-index-set!    vertex index)
     (vertex-lowpoint-set! vertex index)
     (vertex-stack-set!    vertex stack)
     (get-strong vertex index vertex comps)))

 ;; 3

 (define (get-strong vertex index stack comps)
   (if (null? (vertex-edges vertex))
       (end-vertex    vertex index stack comps)
       (follow-edge vertex index stack comps)))

 ;; 7

 (define (end-vertex vertex index stack comps)
   (call-with-values
       (lambda ()
         (if (= (vertex-index vertex) (vertex-lowpoint vertex))
             (unwind-stack vertex stack comps)
             (values stack comps)))
     (lambda (stack comps)
       (cond ((vertex-parent vertex)
              => (lambda (parent)
                   (if (> (vertex-lowpoint parent) (vertex-lowpoint vertex))
                       (vertex-lowpoint-set! parent (vertex-lowpoint vertex)))
                   (get-strong parent index stack comps)))
             (else
              (values index stack comps))))))

 (define (unwind-stack vertex stack comps)
   (let loop ((n stack) (c '()))
     (let ((next (vertex-stack n))
           (c (cons (vertex-data n) c)))
       (vertex-stack-set! n #f)
       (if (eq? n vertex)
           (values next (cons c comps))
           (loop next c)))))

 ;; 4

 (define (follow-edge vertex index stack comps)
   (let* ((next (pop-vertex-edge! vertex))
          (next-index (vertex-index next)))
     (cond ((= next-index 0)
            (vertex-parent-set! next vertex)
            (do-vertex next index stack comps))
           (else
            (if (and (< next-index (vertex-index vertex))
                     (vertex-stack next)
                     (< next-index (vertex-lowpoint vertex)))
                (vertex-lowpoint-set! vertex next-index))
            (get-strong vertex index stack comps)))))

 (define (pop-vertex-edge! vertex)
   (let ((edges (vertex-edges vertex)))
     (vertex-edges-set! vertex (cdr edges))
     (car edges)))

 ;; GRAPH is ((<symbol> . <symbol>*)*)

 (define (scsh-strongly-connected-components graph)
   (let ((vertices (map (lambda (n)
                          (vector (car n) #f #f))
                        graph)))
     (for-each (lambda (data vertex)
                 (vector-set! vertex 1 (map (lambda (s)
                                              (find (lambda (v)
                                                      (eq? s (vector-ref v 0)))
                                                    vertices))
                                            (cdr data))))
               graph
               vertices)
     (map (lambda (l)
            (map (lambda (n) (vector-ref n 0)) l))
          (strongly-connected-components vertices
                                         (lambda (v) (vector-ref v 1))
                                         (lambda (v) (vector-ref v 2))
                                         (lambda (v val)
                                           (vector-set! v 2 val))))))

 (define (test-strong-auto)
   (let ([graph '((a b) (b c) (c a) (d d))])
     (pretty-print (scsh-strongly-connected-components graph))))

 )