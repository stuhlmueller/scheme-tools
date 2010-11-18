;; $Id: xtest-harness.scm,v 1.2 2005/01/28 09:16:57 lizorkin Exp $
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin


;==============================================================================
; Auxiliary

; A counterpart to list= from SRFI optimized for two lists
(define (xtest-list= = list-a list-b)
  (let rpt ((a list-a) (b list-b))
    (if (null? a)
      (null? b)
      (and (not (null? b))
	   (= (car a) (car b))
	   (rpt (cdr a) (cdr b))))))

; For use in expected results: will be considered as equal to any lambda
(define x-lambda-placeholder (lambda() #t))

; Like equal? but yields #t for comparison of any two procedures.
(define (xtest-equal? x y)
  (cond
    ((and (procedure? x)
	  (procedure? y)))
    ((not (and
	    (list? x)
	    (list? y)))
     (equal? x y))
    (else 
      (xtest-list=
	(lambda (x y) 
	  (xtest-equal? x y))
	x y)
      )))


;==============================================================================
; Test assertions verificators

(define xtest-sep-line "  ==============  ")

; Test assertion: (equal? `(,selector ,@params) expected-result) 
(define-macro (xtest-assert expected-result selector . params)
  (let ((res (gensym)))
    `(begin
       (cerr "-- " ',selector nl 
	     (lambda(port) 
	       (for-each (lambda(p)
			   (xtest-ppw p port))
			 (list ,@params))))
       (let ((,res (,selector ,@params)))
	 (if (equal? ,res ,expected-result)
	   (cerr "->" nl
		 (lambda (port) (pp ,res port)) nl)
	   (begin
	     (cerr
              "Unexpected result: " nl
              (lambda (port) (pp ,res port)))
             (cerr
              "Expected: " nl
              (lambda (port) (pp ,expected-result port)))
             (cerr
              "See the difference: " nl
              (lambda (port) (pp (xtest:diff ,res ,expected-result) port)))
	     (exit -1)))))))
	
; Test assertion: 
; (begin
;   `(,selector ,@params) 
;   (equal? var expected-result)) 
(define-macro (xtest-assert-var var expected-result selector . params)
  (let ((res (gensym)))
    `(begin
       (let ((xtest--var ,var))
       (cerr nl xtest-sep-line ',selector xtest-sep-line
	     nl "(" ',selector nl 
	     (lambda(port) 
	       (for-each (lambda(p)
			   (xtest-ppw p port))
			 (list ,@params)))
	      ")")
         (,selector ,@params)
	 (cerr nl ";--[var]-->" nl
		 (lambda (port) (pp xtest--var port)))
	 (or (xtest-equal? ,expected-result xtest--var)
	   (begin
	     (cerr  
		 "### Expected:" nl
		 (lambda (port) (pp ,expected-result port))
		 "### ASSERTION FAILED for: " ',selector nl) 
	     (exit -1)))))))

; Test assertion: (equal? `(,selector ,@params) expected-result) 
(define-macro (xtest-assert-write expected-result selector . params)
  (let ((res (gensym)))
    `(begin
       (cerr nl xtest-sep-line ',selector xtest-sep-line  
	     nl "(" ',selector nl 
	     (lambda(port) 
	       (for-each (lambda(p)
			   (xtest-ppw p port))
			 (list ,@params)))
	     ")")
       (let ((,res (,selector ,@params)))
	   (cerr nl ";---->" nl
		 (lambda (port) (write ,res port)))
	 (or (xtest-equal? ,res ,expected-result)
	   (begin
	     (cerr 
		 "### Expected:" nl
		 (lambda (port) (write ,expected-result port))
		 "### ASSERTION FAILED for: " ',selector nl)
	     (exit -1)))))))


;==========================================================================
; Diff

; Returns '() if objects are equal
; Differences found:
; (listof '(diff (sxpath ...) (comment ...) (first ...) (second ...))
(define (xtest:diff obj1 obj2)
  (letrec
      ((form-diff
        ; Forms a difference element
        (lambda (node1 node2 lpath comment)
          `(diff
            (sxpath ,(reverse lpath))
            (comment ,comment)
            (result ,node1)
            (expected ,node2))))
       (tree-walk
        ; lpath - reverse location path to current nodes
        ; Returns diffs found or '()
        (lambda (node1 node2 lpath)
          (cond
            ((or (not (pair? node1))
                 (not (pair? node2)))  ; either node is atomic
             (if (equal? node1 node2)
                 '()
                 (list
                  (form-diff node1 node2 lpath "different atoms"))))            
            ((not (equal? (car node1) (car node2)))  ; different element names
             (list
              (form-diff node1 node2 lpath "different operations")))
            ((not (= (length node1)
                     (length node2)))  ; different number of arguments
             (list
              (form-diff node1 node2 lpath "different number of arguments")))
            (else  ; recursive
             (apply
              append
              (map
               (lambda (kid1 kid2)
                 (tree-walk kid1 kid2 (cons (car node1) lpath)))
               (cdr node1) (cdr node2))))))))
    (tree-walk obj1 obj2 '())))
