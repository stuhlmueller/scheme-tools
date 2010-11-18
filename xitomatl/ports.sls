#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl ports)
  (export
    binary-input-port? binary-output-port?
    textual-input-port? textual-output-port?
    port-closed?  ;; from (xitomatl ports compat)
    read-all get-lines-all
    port-for-each port-map 
    input-port-enumerator  ;; from (xitomatl enumerators)
    open-binary-compound-input-port open-textual-compound-input-port
    #|open-binary-pipe-ports open-textual-pipe-ports|#)
  (import
    (rnrs)
    (only (xitomatl define) define/AV)
    (only (xitomatl control) begin0)
    #|(only (xitomatl bytevectors) subbytevector)
    (only (xitomatl strings) string-copy!)
    (xitomatl queue)|#
    (only (xitomatl enumerators) fold/enumerator input-port-enumerator)
    (xitomatl ports compat))
  
  (define (binary-input-port? x)
    (and (input-port? x)
         (binary-port? x)))
  (define (binary-output-port? x)
    (and (output-port? x)
         (binary-port? x)))
  (define (textual-input-port? x)
    (and (input-port? x)
         (textual-port? x)))
  (define (textual-output-port? x)
    (and (output-port? x)
         (textual-port? x)))

  (define read-all
    (case-lambda 
      ((port)
       (port-map values read port))
      (()
       (read-all (current-input-port)))))  
  
  (define get-lines-all
    (case-lambda
      ((port)
       (port-map values get-line port))
      (()
       (get-lines-all (current-input-port)))))
  
  (define port-for-each
    (case-lambda
      ((proc reader port)
       (fold/enumerator
        (input-port-enumerator reader)
        port
        (lambda (x) (proc x) #T)))
      ((proc reader) 
       (port-for-each proc reader (current-input-port)))))
  
  (define port-map
    (case-lambda
      ((proc reader port)
       (reverse
        (fold/enumerator
         (input-port-enumerator reader)
         port
         (lambda (x a) (values #T (cons (proc x) a)))
         '())))
      ((proc reader) 
       (port-map proc reader (current-input-port)))))
  
  (define/AV (open-compound-input-port list-or-proc maybe-transcoder)
    ;; A compound input port is a custom port which represents the logical
    ;; concatenation of other input ports.  It starts out with an ordered
    ;; collection of input ports and reads from the first one until end of file
    ;; is reached, whereupon it reads from the second one, and so on, until end
    ;; of file is reached on the last of the contained input ports, and then
    ;; subsequent reads from the compound input port will return end of file.
    ;; After each component port is exhausted, it is closed.  Closing a compound
    ;; input port closes all remaining component ports.  get-position and
    ;; set-position! are not supported.
    ;; 
    ;; The first argument to open-compound-input-port must be either a list of
    ;; components or a zero-argument procedure which returns components.  If it
    ;; is a procedure, it is called each time the next component port is needed
    ;; and it must return the next component or #F to indicate there are no
    ;; more.  The second argument must be either a transcoder or #F.  If it is a
    ;; transcoder, the compound input port will be textual and the acceptable
    ;; values for components are textual input ports, binary input ports,
    ;; strings, and bytevectors; otherwise the compound input port will be
    ;; binary and the acceptable values are binary input ports and bytevectors.
    ;; For a textual compound input port, binary input port components are
    ;; transcoded, bytevector components are used as the source of transcoded
    ;; bytevector input ports, textual input ports are used directly and their
    ;; transcoder may be different than the compound input port's, and string
    ;; components are used as the source of string input ports.  For a binary
    ;; compound input port, binary input port components are used directly and
    ;; bytevector components are used as the source of raw bytevector input
    ;; ports.
    (define (make-handler prefix)
      ;; Returns a function which maps the allowable types into input-ports.
      (define (invalid suffix x)
        (AV (string-append prefix " " suffix) x))
      (if maybe-transcoder
        (lambda (n)
          (cond ((input-port? n) 
                 (if (textual-port? n) n (transcoded-port n maybe-transcoder)))
                ((string? n) 
                 (open-string-input-port n))
                ((bytevector? n)
                 (open-bytevector-input-port n maybe-transcoder))
                (else 
                 (invalid "Not an input port or string or bytevector." n))))
        (lambda (n)
          (cond ((and (input-port? n) (binary-port? n))
                 n)
                ((bytevector? n)
                 (open-bytevector-input-port n))
                (else
                 (invalid "Not a binary input port or bytevector." n))))))
    (define next
      ;; Returns the next input-port, or #F if there are no more.
      (cond 
        ((or (pair? list-or-proc) (null? list-or-proc))
         (let ((l list-or-proc)
               (handle (make-handler "Invalid value in supplied list.")))
           (lambda ()
             (cond ((pair? l) (begin0 (handle (car l))
                                      (set! l (cdr l))))
                   ((null? l) #F)
                   (else (AV "not a proper list" list-or-proc))))))
        ((procedure? list-or-proc)
         (let ((handle 
                (make-handler "Invalid value returned from supplied procedure.")))
           (lambda ()
             (let ((n (list-or-proc)))
               (and n (handle n))))))
        (else 
         (AV "not a list or procedure" list-or-proc))))
    (define (make-compound-port make-custom id get-n! current)
      (make-custom id
        (letrec ((read! (lambda (str-or-bv start count)
                          (if current
                            (let ((x (get-n! current str-or-bv start count)))
                              (cond ((eof-object? x)
                                     (close-port current)
                                     (set! current (next))
                                     (read! str-or-bv start count))
                                    (else
                                     x)))
                            0 #| EOF for this compound port |#))))
          read!)
        #F #F       ;; get-position and set-position! not supported
        (lambda ()
          ;; This `when' also prevents a finished `next'
          ;; from being called more than once.
          (when current
            (close-port current)
            (set! current #F)  ;; shouldn't be necessary, but just in case
            (let loop ((n (next)))
              (when n
                (close-port n)
                (loop (next))))))))
    (let ((current (next)))
      (if maybe-transcoder
        (make-compound-port 
          make-custom-textual-input-port
          "<textual-compound-input-port>"
          get-string-n!
          current)
        (make-compound-port 
          make-custom-binary-input-port
          "<binary-compound-input-port>"
          get-bytevector-n!
          current))))
  
  (define (open-binary-compound-input-port list-or-proc)
    (open-compound-input-port list-or-proc #F))
  
  (define/AV open-textual-compound-input-port
    (case-lambda
      ((list-or-proc)
       (open-compound-input-port list-or-proc (native-transcoder)))
      ((list-or-proc transcoder)
       (unless transcoder
         (AV "transcoder cannot be #F"))
       (open-compound-input-port list-or-proc transcoder))))
#|  
  (define (make-open-pipe-ports mcop opid mcip ipid sub copy! len)
    ;; TODO: Need a mutex for each pipe so that it can be made safe for a
    ;;       thread to use one of a pipe's ports and another thread to use the
    ;;       other port.  Need a way for the input port to block until there 
    ;;       is something ready to be read.
    (begin  ;; Just for superficial non-threaded testing
      (define (make-mutex) #F)
      (define (acquire-mutex m) (values))
      (define (release-mutex m) (values))
      (define-syntax synchronized (syntax-rules () ((_ _ expr ...) (begin expr ...))))
      (define (block-until-something-enqueued) (values)))
    ;; NOTE: The safety of concurrent use of the same port is not the 
    ;;       responsibility of this pipes implementation.
    (lambda ()
      (let ((mutex (make-mutex))
            (q (make-empty-queue))
            (closed #F))
        (values
         (mcop opid
          (lambda (bv-or-str start count)
            (let ((x (if (positive? count)
                       (sub bv-or-str start (+ start count))
                       (eof-object))))
              (synchronized mutex
                (when closed
                  (assertion-violation opid "input end closed"))
                (enqueue! q x)))
            count)
          #F #F  ;; get-position and set-position! not supported
          (lambda ()
            (synchronized mutex
              (set! closed #T))))
         (let ((current #F) (pos #F))
           (mcip ipid
            (letrec ((read!
                      (lambda (bv-or-str start count)
                        (if current
                          (let* ((curlen (- (len current) pos))
                                 (copylen (min count curlen)))
                            (copy! current pos bv-or-str start copylen)
                            (if (= curlen copylen)
                              (begin
                                (set! current #F)
                                (set! pos #F))
                              (set! pos (+ pos copylen)))
                            copylen)
                          (if (begin (acquire-mutex mutex)
                                     (positive? (queue-length q)))
                            (let ((x (dequeue! q)))
                              (release-mutex mutex)
                              (if (eof-object? x)
                                0  ;; EOF, but still possible to read again
                                (begin (set! current x)
                                       (set! pos 0)
                                       (read! bv-or-str start count))))
                            (if (begin0 closed
                                        (release-mutex mutex))
                              0  ;; return EOF from now on
                              (begin (block-until-something-enqueued)
                                     (read! bv-or-str start count))))))))
              read!)
            #F #F  ;; get-position and set-position! not supported
            (lambda ()
              (synchronized mutex 
                (set! q #F)
                (set! closed #T))
              (set! current #F))))))))
  
  (define open-binary-pipe-ports
    (make-open-pipe-ports 
     make-custom-binary-output-port "<binary-pipe-output-port>"
     make-custom-binary-input-port "<binary-pipe-input-port>"
     subbytevector
     bytevector-copy!
     bytevector-length))
  
  (define open-textual-pipe-ports
    (make-open-pipe-ports 
     make-custom-textual-output-port "<textual-pipe-output-port>"
     make-custom-textual-input-port "<textual-pipe-input-port>"
     substring
     string-copy!
     string-length))
|# 
  ;; TODO: Pushback ports
  
  ;; TODO?: Filter ports
  
  ;; TODO?: Line counting ports
  
)
