#!r6rs

;; Associate arbitrary properties with objects.

(library

 (scheme-tools property)

 (export set-property!
         get-property
         get/set-property)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (scheme-tools readable-scheme)
         (scheme-tools table))

 (define object-properties (make-table eq?))

 (define (get-property-table obj)
   (table-lookup/set! object-properties obj (lambda () (make-table eq?))))
 
 (define (set-property! obj property value)
   (let ([table (get-property-table obj)])
     (table-add! table property value)))

 (define (get-property obj property . default)
   (let ([table (get-property-table obj)])
     (table-lookup table property (if (null? default)
                                      (lambda () (error (pair obj property) "property not found"))
                                      (first default)))))

 (define (get/set-property obj property make-value)
   (get-property obj property
                 (lambda () (let ([value (make-value)])
                         (set-property! obj property value)
                         value))))

 )