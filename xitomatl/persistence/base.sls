#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl persistence base)
  (export
    serialize-object-field deserialize-object-field
    serialize-object deserialize-object
    store-object-field load-object-field
    store-object load-object
    new-persistent-objects-directory new-persistent-type new-persistent-object)
  (import
    (rnrs)
    (rnrs eval)
    (only (xitomatl file-system base) make-directory)
    (xitomatl file-system paths))
  
  (define next-id-file "next-id")
  (define type-of-types-id "0")
  (define first-id 1)
  (define objects-dir "object")
  (define all-field "all")
  (define type-field "type")
  (define serializer-postfix "-serializer")
  (define deserializer-postfix "-deserializer")
  
  (define serializing-source-code-environment
    ;; NOTE: This can't prevent excessive memory or CPU usage.
    (environment '(xitomatl rnrs-restricted)
                 '(xitomatl persistence transcoded-serializing)))
  
  (define (serialize-object-field bd serializer id field x)
    (let ((p (open-file-output-port (path-join bd objects-dir id field) (file-options no-fail))))
      (dynamic-wind
        values
        (lambda () (serializer x p) (values))
        (lambda () (close-port p)))))
  
  (define (deserialize-object-field bd deserializer id field)
    (let ((p (open-file-input-port (path-join bd objects-dir id field))))
      (dynamic-wind
        values
        (lambda () (deserializer p))
        (lambda () (close-port p)))))
    
  (define (serialize-object bd serializer id x)
    (serialize-object-field bd serializer id all-field x))
  
  (define (deserialize-object bd deserializer id)
    (deserialize-object-field bd deserializer id all-field))
  
  (define (store-object-field bd id field x)
    ;; TODO: probably cache the serializer
    (let* ((type-id (call-with-input-file (path-join bd objects-dir id type-field) read))
           (code (call-with-input-file (path-join bd objects-dir type-id
                                                  (string-append field serializer-postfix))
                   read))
           (serializer (eval code serializing-source-code-environment)))
      (serialize-object-field bd serializer id field x)))
  
  (define (load-object-field bd id field)
    ;; TODO: probably cache the deserializer
    (let* ((type-id (call-with-input-file (path-join bd objects-dir id type-field) read))
           (code (call-with-input-file (path-join bd objects-dir type-id 
                                                  (string-append field deserializer-postfix))
                   read))
           (deserializer (eval code serializing-source-code-environment)))
      (deserialize-object-field bd deserializer id field)))
  
  (define (store-object bd id x)
    (store-object-field bd id all-field x))
  
  (define (load-object bd id)
    (load-object-field bd id all-field))
  
  (define (new-persistent-type bd fields/serializing-code)
    (let* ((id (next-id bd))
           (td (path-join bd objects-dir id)))
      (make-directory td)
      (call-with-output-file (path-join td type-field)
        (lambda (fop) (write type-of-types-id fop)))
      (for-each 
        (lambda (f.sc)
          (let ((field (car f.sc))
                (serializer-code (cadr f.sc))
                (deserializer-code (caddr f.sc)))
            (call-with-output-file (path-join td (string-append field serializer-postfix))
              (lambda (fop) (write serializer-code fop)))
            (call-with-output-file (path-join td (string-append field deserializer-postfix))
              (lambda (fop) (write deserializer-code fop)))))
        fields/serializing-code)
      id))
  
  (define (next-id bd)
    (let* ((fn (path-join bd next-id-file))
           (nid (call-with-input-file fn read)))
      (call-with-port (open-file-output-port fn (file-options no-create) 
                                             (buffer-mode block) (native-transcoder)) 
        (lambda (fop) (write (+ 1 nid) fop)))
      (number->string nid)))
  
  (define (new-persistent-objects-directory dir-path)
    (make-directory dir-path)
    (make-directory (path-join dir-path objects-dir))
    (make-directory (path-join dir-path objects-dir type-of-types-id))    
    (call-with-output-file (path-join dir-path next-id-file)
      (lambda (fop) (write first-id fop))))
  
  (define (new-persistent-object bd type-id)
    (let* ((id (next-id bd))
           (od (path-join bd objects-dir id)))
      (make-directory od)
      (call-with-output-file (path-join od type-field)
        (lambda (fop) (write type-id fop)))
      id))

)
