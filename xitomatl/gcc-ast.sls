#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

;; This library takes the output of GCC's -fdump-translation-unit option.
;; The GCC man page is misleading saying this option is for C++ only.
;; This option outputs an abstract syntax tree of gcc's input.
;; This library was made for include-gtk.c.t00.tu output by GCC 4.1.2
;; I'm not sure if this would work for different GCC versions.
  
;; NOTE: This might still need to be updated to work with IrRegex.  

(library (xitomatl gcc-ast)
  (export
    read-node-lines
    read-node-lines-from-file
    node-lines->graph
    find-all-function_decl/name-prefixes->func-specs
    find-all-enumeral_type/name-prefixes->enum-specs)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (xitomatl common) format)
    (xitomatl irregex)
    (xitomatl irregex extras)
    (xitomatl IU-match))
  
  (define-syntax dpf
    (syntax-rules ()
      #;((_ . rest) (printf . rest))
      ((_ . rest) (values))))
  
  
  (define (read-node-lines ip)
    ;; Returns list of strings.
    ;; Each string is the entire textual syntax for a node.
    (define (read-one-node/string)
      (define (return a)
        (if (null? a)
          #F
          (apply string-append (reverse a))))
      (let loop ((accum '()))
        (define l (get-line ip))
        (if (eof-object? l)
          (return accum)
          (let ((a (cons l accum)))
            (if (equal? #\@ (peek-char ip)) 
              (return a)
              (loop a))))))    
    (let loop ((accum '()))
      (define node/string (read-one-node/string))
      (if node/string
        (loop (cons node/string accum))
        (reverse accum))))
  
  (define (read-node-lines-from-file input-filename)
    (call-with-input-file input-filename read-node-lines))
  
  (define (node-id n) (caar n))
  (define (node-type n) (cdar n))
  (define (node-attrs n) (cdr n))
  
  (define (node-lines->graph node-lines)
    (define graph (make-vector (+ 1 (length node-lines)) #F))
    ;; regexp match node-line
    (do ((i 1 (+ 1 i))  ;; Ignore 0, start at 1, because gcc-tu's output indexing starts at 1
         (nls node-lines (cdr nls)))
      ((= i (vector-length graph)))
      (let* ((nm (irregex-search "@(\\d+)\\s+(\\w+)\\s+(.*)" (car nls)))
             (id-num (string->number (irregex-match-substring nm 1)))
             (type (string->symbol (irregex-match-substring nm 2)))
             (attrs (map (lambda (am) 
                           (cons (string->symbol (irregex-match-substring am 1))
                                 (irregex-match-substring am 2))) 
                         (irregex-search/all "(\\w+|op\\s+\\d+)\\s*:\\s*(\\S+(?:\\s+\\S+)*?)(?=(?:\\s+\\w+\\s*:)|(?:\\s+bitfield)|(?:\\s+op\\s+\\d+)|(?:\\s*$))"
                           (irregex-match-substring nm 3)))))
        ;; Construct node data-structures a-list of shape:
        ;;   ((<node-number-id> . <type>) (<attribute> . <value>) ...)
        (vector-set! graph i (cons (cons id-num type) attrs))))
    ;; Scan through the nodes' attributes and replace <value>s of "@\d+" 
    ;; with memory references to the nodes identified by 
    ;; the numbers, and return the updated graph
    (do ((i 1 (+ 1 i)))
      ((= i (vector-length graph))
       graph)
      (let ((node (vector-ref graph i)))
        (for-each
          (lambda (attr+val*)
            (define val (cdr attr+val*))
            (when (char=? #\@ (string-ref val 0))
              (set-cdr! attr+val*
                (vector-ref graph
                  (string->number (substring val 1 (string-length val)))))))
          (cdr node)))))
  
  (define (attr-val node field)
    (cond ((assoc field (node-attrs node)) => cdr)
          (else (error 'attr-val "attrs does not have field" field))))
  
  (define (func-spec-name fs) (cdr (assoc 'name fs)))
  (define (func-spec-return-type fs) (cdr (assoc 'return-type fs)))
  (define (func-spec-arguments-types fs) (cdr (assoc 'arguments-types fs)))
  
  
  (define (function_decl->func-spec function_decl-node)
    ;; Returns data-structure of shape:
    ;;   ((name . <name-str>) 
    ;;    (return-type . <type-spec>) 
    ;;    (arguments-types . (<type-spec> ...)))
    ;; Where <type-spec> is one of:
    ;;   (void "void")
    ;;   (integer <type-name-str>)
    ;;   (boolean <type-name-str>)
    ;;   (enumeral <type-name-str>)
    ;;   (real <type-name-str>)
    ;;   (complex <type-name-str>)
    ;;   (union <type-name-str>)
    ;;   (pointer (struct <type-name-str>))
    ;;   (pointer function)
    ;;   (pointer <type-spec>)
    
    (define (problem who msg . args)
      (apply error 'function_decl->func-spec
             (format "(Node# ~a) ~a: ~a" (node-id function_decl-node) who msg) 
             args))
    
    (define (function_decl-name-str fdn)
      (attr-val (attr-val fdn 'name) 'strg))
    
    (define (type-node->type-spec node)
      (define (type-node-name-str n)
        (dpf "   type-node-name-str (Node# ~a)\n" (node-id n))
        (let ((nn (attr-val n 'name)))
          (case (node-type nn)
            ((identifier_node) (attr-val nn 'strg))
            ((type_decl) (attr-val (attr-val nn 'name) 'strg))
            (else (problem 'type-node->type-spec/nnt "don't know" (node-type nn))))))
      (define (make-spec n)
        (cond
          ((assoc 'name (node-attrs n))
           (case (node-type n)
             ((void_type real_type integer_type complex_type 
               enumeral_type boolean_type union_type)
              (let* ((nts (symbol->string (node-type n)))
                     (nts/w/o (substring nts 0 (- (string-length nts) 5))))
                (list (string->symbol nts/w/o) (type-node-name-str n))))
             (else
              (problem 'type-node->type-spec/nt "don't know" (node-type n)))))
          ((assoc 'unql (node-attrs n))
           => (lambda (up) (make-spec (cdr up))))
          (else "*UNNAMED*")))
      (dpf "  type-node->type-spec (Node# ~a) ~s\n" 
              (node-id node)
              (map car (node-attrs node)))
      (case (node-type node)
        ((pointer_type)
         (let ((ptd (attr-val node 'ptd)))
           (case (node-type ptd)
             ((record_type)
              `(pointer (struct ,(type-node-name-str ptd))))
             ((function_type)
              ;; This meets the needs of discovering compatible GTK function prototypes.
              ;; More info about the function type could be extracted.
              '(pointer function))
             ((pointer_type)
              `(pointer ,(type-node->type-spec ptd)))
             (else
              `(pointer ,(make-spec ptd))))))        
        ((reference_type)  ;; there's only one of these in all of include-gtk.c.t00.tu
         `(reference ,(type-node->type-spec (attr-val node 'refd))))
        (else
         (make-spec node))))
    
    (define (function_decl-return-type fdn)
      (define retn (attr-val (attr-val fdn 'type) 'retn))
      (dpf " function_decl-return-type\n")
      (type-node->type-spec retn))
    
    (define (function_decl-arguments-types fdn)
      (define (tree_list->node-list n)
        (let* ((valu (attr-val n 'valu))
               (chan-pair (assoc 'chan (node-attrs n))))
          (if chan-pair
            (cons valu (tree_list->node-list (cdr chan-pair)))
            (list valu))))
      (dpf " function_decl-arguments-types\n")
      (cond
        ((assoc 'prms (attr-val fdn 'type))
         => (lambda (prms-pair)
              (let ((prms (cdr prms-pair)))
                (case (node-type prms)
                  ((tree_list)
                   (let ((ts (map type-node->type-spec (tree_list->node-list prms))))
                     ;; don't include the last element when it's void
                     (if (and (positive? (length ts))
                              (match (list-ref ts (- (length ts) 1))
                                ((void . ,rest) #T)
                                (,else #F)))
                       (reverse (cdr (reverse ts)))
                       ts)))
                  (else
                   (problem 'function_decl-arguments-types/prms-type "don't know yet" (node-type prms)))))))
        (else '())))
    
    (dpf "\nfunction_decl->func-spec: (Node# ~a) ~a ~s\n" 
            (node-id function_decl-node)
            (function_decl-name-str function_decl-node)
            (map car (node-attrs function_decl-node)))
    
    (let ((ret
           `((name . ,(function_decl-name-str function_decl-node))
             (return-type . ,(function_decl-return-type function_decl-node))
             (arguments-types . ,(function_decl-arguments-types function_decl-node)))))
      (dpf "=> function_decl->func-spec =>\n")
      ret))
  
  
  (define (find-all-function_decl/name-prefixes->func-specs graph name-prefixes)
    ;; graph must be a vector of nodes, as returned by node-lines->graph
    ;; name-prefixes must be a list of strings.
    (filter 
      (lambda (func-spec)
        (define func-name (func-spec-name func-spec))
        (exists
          (lambda (prefix) 
            (and (> (string-length func-name) (string-length prefix))
                 (string=? prefix (substring func-name 0 (string-length prefix)))))
          name-prefixes))
      (map 
        function_decl->func-spec
        (filter  ;; only want function_decl nodes
          (lambda (node) (symbol=? (node-type node) 'function_decl))
          (cdr (vector->list graph)))))) ;; vector-ref 0 is always ignored
  
  
  (define (enum-spec-type-name enum-spec) (cdr (assoc 'type-name enum-spec)))
  (define (enum-spec-members enum-spec) (cdr (assoc 'members enum-spec)))
  
  (define (enumeral_type->enum-spec enumeral_type-node)
    ;; Returns data-structure of shape:
    ;;   ((type-name . <name-str>)
    ;;    (members . ((<name-str> . <integer>) ...)))
    
    (define (enumeral_type-type-name etn)
      (let ((x (attr-val etn 'name)))
        (case (node-type x)
          ((identifier_node)           
           (attr-val x 'strg))
          ((type_decl)
           (attr-val (attr-val x 'name) 'strg))
          (else
           (error 'enumeral_type-type-name "don't know node-type" (node-type x))))))
    
    (define (enumeral_type-members etn)
      (define (tree_list->members-specs n)
        (let* ((name (attr-val (attr-val n 'purp) 'strg))
               (i (attr-val (attr-val n 'valu) 'low))
               (name+i (cons name i))
               (chan-pair (assoc 'chan (node-attrs n))))
          (if chan-pair
            (cons name+i (tree_list->members-specs (cdr chan-pair)))
            (list name+i))))
      (tree_list->members-specs (attr-val etn 'csts)))
    
    (dpf "\nenumeral_type->enum-spec: (Node# ~a) ~a ~s\n" 
         (node-id enumeral_type-node)
         (enumeral_type-type-name enumeral_type-node)
         (map car (node-attrs enumeral_type-node)))
    
    (let ((ret
           `((type-name . ,(enumeral_type-type-name enumeral_type-node))
             (members . ,(enumeral_type-members enumeral_type-node)))))
      (dpf "=> enumeral_type->enum-spec =>\n")
      ret))
  
  
  (define (find-all-enumeral_type/name-prefixes->enum-specs graph name-prefixes)
    ;; graph must be a vector of nodes, as returned by node-lines->graph.
    ;; name-prefixes must be a list of strings.
    (filter 
      (lambda (enum-spec)
        (define enum-type-name (enum-spec-type-name enum-spec))
        (exists
          (lambda (prefix) 
            (and (> (string-length enum-type-name) (string-length prefix))
                 (string=? prefix (substring enum-type-name 0 (string-length prefix)))))
          name-prefixes))
      (map 
        (lambda (node) 
          (with-exception-handler
            (lambda (ex)
              (raise (condition (make-who-condition 'enumeral_type->enum-spec)
                                (make-message-condition (format "exception on (Node #~a)" (node-id node)))
                                ex)))
            (lambda () (enumeral_type->enum-spec node))))
        (filter  ;; only want enumeral_type nodes with name field
          (lambda (node) 
            (and (symbol=? (node-type node) 'enumeral_type)
                 (assoc 'name (node-attrs node))))
          (cdr (vector->list graph)))))) ;; vector-ref 0 is always ignored
  
)
