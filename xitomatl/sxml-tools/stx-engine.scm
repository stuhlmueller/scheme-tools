;; $Id: stx-engine.scm,v 1.9502 2004/01/22 01:11:45 kl Exp kl $

;=============================================================================
; Auxilliary 

(define stx:version 
  (string-append " $Revision: 1.9502 $" nl " $Date: 2004/01/22 01:11:45 $"))

#;(define (stx:error . messages)
  (cerr nl "STX: ")
  (apply cerr messages)
  (cerr nl)
  (exit -1))


;=============================================================================
; Syntactic sugar 

; This macro emulates <xsl:apply-template> of XSLT
; "select" must be a node-set
; If omitted, (sxml:content current-node) is used.  
#;(define-macro (xsl:apply-templates . select)
  (cond  
    ((null? select)
     `(stx:apply-templates (sxml:content current-node) 
			   stx:templates current-root $))
    ((null? (cdr select))
    `(stx:apply-templates ,(car select) stx:templates current-root $))
    (else (myenv:error "Invalid parameters for 'xsl:apply-template': " select))))

;------------------------------------------------------------------------------
; These macros provide support for abbreviated stylesheets:
;
;  <Stylesheet> ::= (stx:stylesheet <Template>+)
;  <Template>   ::= (match <SXPath> <Handler>)
;  <SXPath>     ::= SXPath expression
;  <Handler>    ::= (lambda (current-node stx:templates current-root $) ...)
; 
; For example:
;  (stx:stylesheet
;    (match "//element[state/@condition='standard']"
;	   (lambda (current-node stx:templates current-root $)
;	     (sxml:text current-node)))
;    (match (table (tr 4))
;	   (lambda (current-node stx:templates current-root $)
;	     `(ol
;		,@(map 
;		    (lambda(x) `(li ,x))
;		,((sxpath '(td *TEXT*)) current-node))))))

(define-syntax  sxml:stylesheet
   (syntax-rules  ()
 		 ((stx rule ...)
		  (list 
		    ; default handler
		    (list '*DEFAULT* 
			  (lambda (node bindings root environment)
			     (stx:apply-templates (sxml:content node) 
						  bindings 
						  root environment)
			     ))
		    ; handler for textual nodes
		    (list '*TEXT* 
			  (lambda(text) text)) 
		    rule ...))))

(define-syntax  match
   (syntax-rules  ()
 		 ((match pattern handler)
		   (list (if (symbol? pattern) pattern (sxpath pattern))
			   handler))
		 ))


;=============================================================================
; Tree transformation

; stx:apply-templates:: <tree> x <templates> x <root> x <environment> -> <new-tree>
; where
; <templates> ::= <default-template> <text-template> <template>*
; <default-template> ::= (*DEFAULT* . <handler>)
; <text-template> ::= (*TEXT* . <handler>)
; <template>  ::= (<matcher> <handler>) | ( XMLname <handler>)
; <root>     ::= <document-root>
; <environment> ::= <lambda-tuple>
; <matcher>  ::= <node> <root> -> <nodeset>
; <handler> :: <node> <templates> <root> <environment> -> <new-node>
;
; The stx:apply-templates function visits top-level nodes of a given tree and 
; process them in accordance with a list of templates given. 
; If a node is a textual one then it is processed usind 'text-template',
; which has to be second element in given list of templates. 
; If a node is a pair then stx:apply-templates looks up a corresponding template
; among  given <templates> using stx:find-template function. 
; If failed, stx:apply-templates tries to locate a *DEFAULT* template, 
; which has to be first element in given list of templates. It's an
; error if this latter attempt fails as well.  
; Having found a template, its handler is applied to the current node. 
; The result of the handler application, which should
; also be a <tree>, replaces the current node in output tree.
;
; This function is slightly similar to Oleg Kiselyov's "pre-post-order" function
; with *PREORDER* bindings. 
(define (stx:apply-templates tree templates root environment)
  (cond
    ((nodeset? tree)
     (map (lambda (a-tree) 
	    (stx:apply-templates a-tree templates root environment)) 
	  tree))
    ((pair? tree) 
     (cond
       (;(tee-4 "Template: " 
	(stx:find-template tree 
		      (cddr templates) ; *DEFAULT* and *TEXT* skipped
		      root);) 
	=> (lambda (template) 
	     ((cadr template) tree templates root environment)))
       (else 
	 (if (eq? '*DEFAULT* (caar templates))
	   ((cadar templates) tree templates root environment)
	   (stx:error "stx:apply-templates: There is no template in: " templates
		      nl "for: " tree
		      )) 
	 )))
    ((string? tree) ; for *TEXT* , simple speed-up - just return 'tree' 
	 (if (eq? '*TEXT* (caadr templates))
	   ((cadadr templates) tree)
	   (stx:error "stx:apply-templates: There is no *TEXT* templates for: " 
		      templates))) 
    (else (stx:error "Unexpected type of node: " tree))))

;  stx:find-template: <node> x <templates> x <root> -> <template>
;  This function returns first template in <templates> whouse <matcher>
;  matches given <node>
;  <matcher> matches node if:
;    - if it is a symbol and its the same as the name of the node matched
;    - if it is a procedure (sxpath/txpath generated one) then it is 
;     applyed (with respect to given <root>) sequentially to the matched node 
;     and its parents until the matched node is a member of a resulting nodeset 
;     or root node is reached. In the first case the node matches successfuly, 
;     in the second case it does not. 
(define (stx:find-template node templates root)
  (let ((pattern-matches? 
	  (lambda (node pattern-test) 
	    (let rpt ((context-node node))
	      (cond 
		((null? context-node) #f)
 ;		((memq node (pattern-test context-node root '()))
		((memq node (pattern-test context-node `((*ROOT* . ,root))))
		 #t)
		(else ; try PARENT
		  (rpt ((sxml:node-parent root) context-node))))))))  
    (let rpt ((bnd templates)) 
      (cond ((null? bnd) #f)
	    ((and (symbol? (caar bnd)) (eq? (caar bnd) (car node)))
	     (car bnd))
	    ((and (procedure? (caar bnd)) ; redundant?
		  (pattern-matches? node (caar bnd)))
	     (car bnd))
	    (else (rpt (cdr bnd)))))))


