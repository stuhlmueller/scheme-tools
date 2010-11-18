;; sxpath+ - a macro implementation of the sxpath function
; $Id: sxpath-plus.scm,v 1.3 2004/01/25 16:14:20 kl Exp kl $
; 
; This macro was proposed by Jim Bender on SSAX-SXML mail list 30/07/2003
; http://sourceforge.net/mailarchive/forum.php?thread_id=2860201&forum_id=599
; 
; This is version is based on "binary" sxpath, where location step functions 
; have two parameters -	 nodeset and a list var bindings (including *ROOT*).
; commited to SourceForge CVS Jan 13 2004
;
; The "look and feel" is similar to the normal sxpath function. Instead of 
;   (sxpath '(div // para))
; one writes 
;   (sxpath+ (div // para))
; 
; Please note that the location path list is not quoted any more!
;
; Like normal sxpath calls, one can unquote to embedded an expression result in 
; the path expression:
;   (sxpath+ (// (chapter (^ (equal? ,a-node)))))
; or to embedded a procedure:
;   (sxpath+ (,(lift (node-parent tree)) ^ name))
; 
; Note: location step functions  are required to take 2 parameters 
; (a node-or-nodeset  and environment of variable bindings). 
; Thus a small convenience "lift" is used, in order to lift 
; a function of type nodeset -> nodeset into one that also accepts 
; (but does not use) 2 parameters: node and environment.
;
; This code does use syntax-case, not just syntax-rules, in three
; places: to distinguish symbols and txpath strings in locations steps, to
; recognize numbers as abbreviations for (node-pos ...), and to match names
; which are bound-identifiers (eq? and equal?).
;

; dispatch on a number (node-pos) reducer pattern
(define-syntax analyze-red1
  (lambda (stx)
    (syntax-case stx ()
		 ((_ num nsenv env)
		  (number? (syntax-object->datum (syntax num)))
		  (syntax (node-pos num))))))
  
; analyze a "reducer"
(define-syntax analyze-reduce
  (syntax-rules ()
		((_ () nsenv env)
		 (sxml:filter (analyze-path () nsenv env)))
		((_ (step ...) nsenv env)
		 (sxml:filter (analyze-path (step ...) nsenv env)))
		((_ item nsenv env)
		 (analyze-red1 item nsenv env))))
  
; dispatch on string or symbol within a location step
(define-syntax analyze-1
  (lambda (stx)
    (syntax-case stx ()
		 ((_ sym nsenv env)
		  (identifier? (syntax sym))
		  (syntax (select-kids (ntype?? 'sym))))
		 ((_ str nsenv env)
		  (string? (syntax-object->datum (syntax str)))
		  (syntax (lambda (nodeset)
			    ((txpath str nsenv) nodeset env)))))))
  
; transform a single location step
(define-syntax analyze-step
  (lambda (stx)
    (syntax-case stx (// *OR* *NOT* ns-id:* unquote)
      ((_ // nsenv env)
       (syntax (node-or (node-self (ntype?? '*ANY*))
			(node-closure (ntype?? '*ANY*)))))
      ((_ (*OR* item ...) nsenv env)
       (syntax (select-kids (ntype-names?? '(item ...)))))
      ((_ (*NOT* item ...) nsenv env)
       (syntax (select-kids (sxml:invert (ntype-names?? '(item ...))))))
      ; eq? and equal? must be matched in a non-hygienic way
      ; in PLT, can use module-or-top-identifier=? for these comparisons
      ((_ (sym-equal? (unquote x)) nsenv env)
       (eq? 'equal? (syntax-object->datum (syntax sym-equal?)))
       (syntax (select-kids (node-equal? x))))
      ((_ (sym-equal? x) nsenv env)
       (eq? 'equal? (syntax-object->datum (syntax sym-equal?)))
       (syntax (select-kids (node-equal? 'x))))
      ((_ (sym-eq? (unquote x)) nsenv env)
       (eq? 'eq? (syntax-object->datum (syntax sym-eq?)))
       (syntax (select-kids (node-eq? x))))
      ((_ (sym-eq? x) nsenv env)
       (eq? 'eq? (syntax-object->datum (syntax sym-eq?)))
       (syntax (select-kids (node-eq? 'x))))
      ((_ (ns-id:* x) nsenv env)
       (syntax (select-kids (ntype-namespace-id?? x))))
      ; exp must evaluate to a procedure (nodeset -> env) -> nodeset
      ; this is done to be consistent with current sxpath function
      ; in the past, this would have been a procedure nodeset -> nodeset
      ((_ (unquote exp) nsenv env)
       (syntax (lambda (nodeset)
		 (exp nodeset env))))
      ((_ ((step ...) reducer ...) nsenv env)
       (syntax (node-reduce (analyze-path (step ...) nsenv env)
			    (analyze-reduce reducer nsenv env) ...)))
      ((_ (() reducer ...) nsenv env)
       (syntax (node-reduce (analyze-path () nsenv env)
			    (analyze-reduce reducer nsenv env) ...)))
      ; this should actually verify that sym is an identifier!!!
      ((_ (sym reducer ...) nsenv env)
       (syntax (node-reduce (analyze-1 sym nsenv env)
			    (analyze-reduce reducer nsenv env) ...)))
      ((_ item nsenv env)
       (syntax (analyze-1 item nsenv env))))))
  
; transform a location path
(define-syntax analyze-path
  (syntax-rules ()
		((_ () nsenv env)
		 (node-join))
		((_ (step ...) nsenv env)
		 (node-join (analyze-step step nsenv env) ...))
		((_ str nsenv env)
		 (analyze-1 str nsenv env))))
  
  (define-syntax sxpath+
    (syntax-rules ()
		  ((_ path)
		   (sxpath+ path '()))
		  ((_ path ns-binding)
		   (let ((nsenv ns-binding))
		     (lambda (node . var-binding)
		       ((analyze-path path nsenv
				      (if (null? var-binding) 
					'()  
					(cadr var-binding))
				      ) node))))))
  
