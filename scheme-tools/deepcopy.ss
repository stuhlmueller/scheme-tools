#!r6rs

;; Vectors and pairs are reconstructed; for other objects (including
;; procedures) references are reused.

(library

 (scheme-tools deepcopy)

 (export deepcopy)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools table)
         (rnrs mutable-pairs)
         (only (srfi :1) iota))

 (define (%deepcopy obj table)
   (cond [(pair? obj)
          (table-lookup table
                        obj
                        (lambda ()
                          (let ([new-pair (pair #f #f)])
                            (table-add! table obj new-pair)
                            (let ([new-car (%deepcopy (car obj) table)]
                                  [new-cdr (%deepcopy (cdr obj) table)])
                              (set-car! new-pair new-car)
                              (set-cdr! new-pair new-cdr)
                              new-pair))))]
         [(vector? obj)
          (table-lookup table
                        obj
                        (lambda () (let ([new-vector (make-vector (vector-length obj))])                                  
                                (table-add! table obj new-vector)
                                (map (lambda (x i) (vector-set! new-vector i (%deepcopy x table)))
                                     (vector->list obj)
                                     (iota (vector-length obj)))
                                new-vector)))]
         [else obj]))

 (define (deepcopy obj)
   (%deepcopy obj (make-table eq?)))

 )