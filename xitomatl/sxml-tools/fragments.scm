;; $Id: fragments.scm,v 1.1 2004/01/20 19:04:43 kl Exp kl $
;   1. sxml:display-fragments is  SRV:send-reply from SXML-tree-trans 
;   2. sxml:clean-fragments is sxml:clean-feed from sxml-tools.scm
;   3. sxml:shtml->http and sxml:fragments->http wrappers for PLT and other
;      web servers

;=============================================================================
; Output the 'fragments'
; The fragments are a list of strings, characters,
; numbers, thunks, #f, #t -- and other fragments.

; This function traverses the tree depth-first, writes out
; strings and characters, executes thunks, and ignores
; #f and '().
; The function returns #t if anything was written at all;
; otherwise the result is #f
; If #t occurs among the fragments, it is not written out
; but causes the result of this function to be #t
(define (sxml:display-fragments . fragments)
  (let loop ((fragments fragments) (result #f))
    (cond
      ((null? fragments) result)
      ((not (car fragments)) (loop (cdr fragments) result))
      ((null? (car fragments)) (loop (cdr fragments) result))
      ((eq? #t (car fragments)) (loop (cdr fragments) #t))
      ((pair? (car fragments))
        (loop (cdr fragments) (loop (car fragments) result)))
      ((procedure? (car fragments))
        ((car fragments))
        (loop (cdr fragments) #t))
      (else
        (display (car fragments))
        (loop (cdr fragments) #t)))))

; Aliases for backward compatibility
(define SRV:send-reply sxml:display-fragments)
(define sxml:display-feed sxml:display-fragments)

; It may be considered as a variant of sxml:display-fragments (SRV:send-reply) 
; While the original function displays fragments, this one returns the list 
; of meaningful fragments and filter out the garbage.
(define (sxml:clean-fragments . fragments)
  (reverse
    (let loop ((fragments fragments) (result '()))
      (cond
	((null? fragments) result)
	((not (car fragments)) (loop (cdr fragments) result))
	((null? (car fragments)) (loop (cdr fragments) result))
	((pair? (car fragments))
	 (loop (cdr fragments) 
	       (loop (car fragments) result)))
	((procedure? (car fragments))
	 (loop (cdr fragments) 
	       (cons ((car fragments)) result)))
	(else
	  (loop (cdr fragments) 
		(cons (car fragments) result)))))))

; Alias for backward compatibility
(define sxml:clean-feed sxml:clean-fragments)

(define (sxml:shtml->http tree)
 (cons "text/html"
   (sxml:clean-fragments
     (sxml:sxml->html tree))))

(define (sxml:fragments->http tree)
 (cons "text/html"
   (sxml:clean-fragments tree)))

