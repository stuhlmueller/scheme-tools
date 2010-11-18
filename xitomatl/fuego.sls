#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

; Fuego -- A prototype object system supporting capability-based security.
;          Inspired by Prometheus by Jorgen Schaefer.

; TODO: Determine the implications for security of information encapsulation
;       w.r.t. record inspecting and record printing.
; TODO: Determine the implications for security w.r.t. exceptions.  
;       I.e. arity errors on methods should not cause an exception which reveals info.
; For _real_ capability-security, these need to be thoroughly analyzed and tamed.

(library (xitomatl fuego)
  (export
    fuego-object? make-key key?
    root-object send #;resend
    ; Distinct key values for root-object's slots 
    :clone :unknown :already-exists :has? :keys
    :add-method! :add-parent! :add-value! :delete! 
    ; Convenient syntaxes    
    object define-values
    ; Exception condition
    fuego-condition? condition-fuego-object)  
  (import
    (rnrs)
    (only (xitomatl define) define/? define-values)
    (only (xitomatl exceptions) assertion-violation/conditions)
    (for (only (xitomatl macro-utils) with-syntax* gen-temp) expand))
  
  ;-----------------------------------------------------------------------------    
  
  (define-record-type fuego-object
    (opaque #T) (sealed #T)
    (fields (mutable slots) (mutable parents)))
    
  (define-record-type key (fields name))  ;; field not used, only for informative printing
  
  ;-----------------------------------------------------------------------------    
  
  (define-condition-type &fuego &condition 
    make-fuego-condition fuego-condition?
    (object condition-fuego-object))
  
  (define (AV/F obj msg . irrts)
    (assertion-violation/conditions "(library (xitomatl fuego))"
                                    msg irrts (make-fuego-condition obj)))
  
  ;-----------------------------------------------------------------------------  
  
  (define/? (send (obj fuego-object?) key . args)
    (find-key/handle obj obj key args #F #F))
  
  (define (resender receiver method-owner)
    (lambda (key . args)
      (let loop ((parents (fuego-object-parents method-owner)))
        (if (null? parents)
          (find-key/handle method-owner method-owner :unknown (cons key args)
            (lambda (ign) (AV/F method-owner "unknown key" key)) #F)
          (find-key/handle receiver (cdar parents) key args loop (cdr parents))))))

  (define (find-key/handle receiver search key args child-loop child-parents)
    ; Search obj's immediate slots
    ; If not found, search parents, depth-first
    (let ((found (assq key (fuego-object-slots search))))
      (if found
        (apply (cdr found) receiver (resender receiver search) args)
        (let loop ((parents (fuego-object-parents search)))
          (if (null? parents)
            (if child-loop
              (child-loop child-parents)
              (find-key/handle receiver receiver :unknown (cons key args)
                (lambda (ign) (AV/F receiver "unknown key" key)) #F))
            (find-key/handle receiver (cdar parents) key args loop (cdr parents)))))))

  ;-----------------------------------------------------------------------------
  
  (define root-clone
    (case-lambda
      ((self resend) (root-clone self resend (make-key 'cloned-parent)))
      ((self resend pk)
       (let ((o (make-fuego-object '() '())))
         (root-add-parent! o #F pk self)
         o))))
  
  (define (root-delete! self resend key)
    (fuego-object-slots-set! self
      (remp (lambda (s) (eq? (car s) key)) 
            (fuego-object-slots self)))
    (fuego-object-parents-set! self
      (remp (lambda (pp) (eq? (car pp) key)) 
            (fuego-object-parents self))))  
  
  (define (root-has? self resend key)
    (if (assq key (fuego-object-slots self)) #T #F))

  (define/? (root-add-method! self resend key (proc procedure?))
    (if (root-has? self #F key)
      (send self :already-exists key proc)
      (fuego-object-slots-set! self
        (cons (cons key proc) (fuego-object-slots self)))))
  
  (define root-add-value! 
    (case-lambda
      ((self resend key val) (root-add-value! self resend key val #F))
      ((self resend key val mutable)
       (define (ina s args) 
         (AV/F s "value method called with invalid number of arguments" (length args)))
       (root-add-method! self #F key
         (if mutable
           (case-lambda 
             ((s r) val) 
             ((s r n) (if (eq? s self)
                        (set! val n)
                        (root-add-value! s #F key n mutable)))
             ((s r . args) (ina s args)))
           (case-lambda 
             ((s r) val)
             ((s r n) (AV/F s "immutable value" key n))
             ((s r . args) (ina s args))))))))
  
  (define/? (root-add-parent! self resend key (obj fuego-object?))
    (let detect ((check (list (cons #F obj))))
      (for-each (lambda (o) 
                  (if (eq? o self)
                    (AV/F self "parent cycle" obj)
                    (detect (fuego-object-parents o))))
                (map cdr check)))
    (root-add-value! self #F key obj)
    (fuego-object-parents-set! self
      (append (fuego-object-parents self) (list (cons key obj)))))
    
  (define (root-keys self resend)
    (map car (fuego-object-slots self)))
  
  (define (root-unknown self resend key . vals)
    (AV/F self "unknown key" key))  
  
  (define (root-already-exists self resend key val)
    (AV/F self "slot already exists" key))  
  
  ;-----------------------------------------------------------------------------    
  
  (define-syntax define-root-keys
    (syntax-rules ()
      ((_ identifier ...)
       (begin (define identifier (make-key 'identifier)) ...))))

  ; Distinct values used as keys used to access standard slots.
  ; Distinct values are used so access to any one of these slots can be 
  ; prevented by not supplying the corresponding value (capability-security).
  (define-root-keys :clone :unknown :already-exists :keys :has? 
                    :add-method! :add-parent! :add-value! :delete!)
  
  (define root-object
    (make-fuego-object 
     (list (cons :clone root-clone)
           (cons :unknown root-unknown)
           (cons :already-exists root-already-exists)
           (cons :keys root-keys)
           (cons :has? root-has?)
           (cons :add-method! root-add-method!)
           (cons :add-parent! root-add-parent!)
           (cons :add-value! root-add-value!)
           (cons :delete! root-delete!))
     '()))
  
  (define-syntax object
    ;; NOTE: Do not re-enter continuations of an object evaluation
    (lambda (stx)
      (syntax-case stx ()
        ((kw body ...)
         (with-syntax ((m (datum->syntax #'kw 'method))
                       (v (datum->syntax #'kw 'value))
                       (p (datum->syntax #'kw 'parent)))
           #'(let ((o (make-fuego-object '() '()))
                   (has-parent #F)
                   (keys '()))
               (let-syntax ((m (syntax-rules () ((_ . r) (M o keys . r))))
                            (v (syntax-rules () ((_ . r) (V o keys . r))))
                            (p (syntax-rules () ((_ . r) (P o keys has-parent . r)))))
                 body ...)
               (unless has-parent
                 (root-add-parent! o #F (make-key 'cloned-parent) root-object))
               (apply values o (reverse keys))))))))
  
  (define-syntax M
    (lambda (stx)
      (syntax-case stx (unquote)
        ((_ o keys (mn s r . a) b0 b ...)
         #'(M o keys mn (lambda (s r . a) b0 b ...)))
        ((_ o keys (unquote mnk) expr) 
         #'(root-add-method! o #F mnk expr))
        ((_ o keys mn expr)
         (syntax-case #'mn (quote)
           ((quote x) (identifier? #'x))
           (x (identifier? #'x)))
         (with-syntax* ((mnk (gen-temp))
                        ((mnk-e add-mnk ...) 
                         (if (identifier? #'mn) 
                           #'((make-key 'mn) (set! keys (cons mnk keys)))
                           #'(mn))))
           #'(let ((mnk mnk-e))
               (M o keys ,mnk expr)
               add-mnk ...))))))
  
  (define-syntax V
    (lambda (stx)
      (syntax-case stx (unquote)
        ((_ o keys vn v) 
         #'(V o keys vn v #F))
        ((_ o keys (unquote vnk) v m)
         #'(root-add-value! o #F vnk v m))
        ((_ o keys vn v m)
         (syntax-case #'vn (quote)
           ((quote x) (identifier? #'x))
           (x (identifier? #'x)))
         (with-syntax* ((vnk (gen-temp))
                        ((vnk-e add-vnk ...) 
                         (if (identifier? #'vn)
                           #'((make-key 'vn) (set! keys (cons vnk keys)))
                           #'(vn))))
           #'(let ((vnk vnk-e))
               (V o keys ,vnk v m)
               add-vnk ...))))))
  
  (define-syntax P
    (lambda (stx)
      (syntax-case stx (unquote)
        ((_ o keys has-parent p) 
         #'(let ((pnk (make-key 'parent))) 
             (P o keys has-parent ,pnk p)))
        ((_ o keys has-parent (unquote pnk) p)
         #'(begin (root-add-parent! o #F pnk p)
                  (set! has-parent #T)))
        ((_ o keys has-parent pn p)
         (syntax-case #'pn (quote)
           ((quote x) (identifier? #'x))
           (x (identifier? #'x)))
         (with-syntax* ((pnk (gen-temp))
                        ((pnk-e add-pnk ...) 
                         (if (identifier? #'pn)
                           #'((make-key 'pn) (set! keys (cons pnk keys)))
                           #'(pn))))
           #'(let ((pnk pnk-e)) 
               (P o keys has-parent ,pnk p)
               add-pnk ...))))))

)
