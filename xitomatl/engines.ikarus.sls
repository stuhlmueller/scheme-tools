;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl engines)
  (export
    make-engine
    engine-return
    mileage 
    round-robin
    por)
  (import
    (rnrs)
    (only (ikarus) engine-handler fxadd1 void die)
    (only (ikarus system $interrupts) $swap-engine-counter!))    
  
  ;; Based off of The Scheme Programming Language engines.
  ;; NOTE: This probably still has issues that need to be determined.
  ;; NOTE: not currently thread safe
  
  (define (start-timer ticks)
    ;; Because of the slight possibility the pcb->engine_counter could run out
    ;; inbetween when we install timer-handler and set the new ticks below
    ;; (which would cause timer-handler, and therefore the current do-expire, to
    ;; be called prematurely), the fuel is manually refilled to ensure there's
    ;; enough.
    ($swap-engine-counter! 1)
    (engine-handler timer-handler)
    ($swap-engine-counter! (fx- ticks)))
  
  (define (reset-state)
    (engine-handler void)
    (set! do-return do-return/oops)
    (set! do-complete do-complete/oops)
    (set! do-expire do-expire/oops))
  
  (define (stop-timer)
    ;; add1 to return value of $swap-engine-counter! to account for the 1 tick
    ;; stop-timer itself needs to consume.  This gives an accurate leftover
    ;; value of 0 to the complete procedures whenever the process had just
    ;; enough fuel to complete and have the engines system finish up.
    (abs (fxmin (fxadd1 ($swap-engine-counter! 1)) 0)))

  (define (do-return/oops args)
    (apply die 'do-return/oops "internal bug" args))
  (define (do-complete/oops ticks vals)
    (apply die 'do-complete/oops "internal bug" ticks vals))
  (define (do-expire/oops resume)
    (die 'do-expire/oops "internal bug" resume))
  
  (define do-return do-return/oops)
  (define do-complete do-complete/oops)
  (define do-expire do-expire/oops)
  
  (define (timer-handler)
    ;; The pcb->engine_counter just passed 0, so there's definitely
    ;; enough fuel for do-expire to reset-state.
    ;;
    ;; NOTE: In order to give a consistent logic to processes which calculated
    ;; they should only need X more ticks to complete, the ticks supplied by
    ;; resume may need to be adjusted, and this may need to be updated as Ikarus
    ;; changes.
    (start-timer (call/cc do-expire)))
  
  (define (new-engine resume)
    (define (engine ticks complete expire)
      (define who "some engine invocation")
      (unless (and (fixnum? ticks) (fxpositive? ticks))
        (die who "not a positive fixnum" ticks))
      (unless (and (procedure? complete) (procedure? expire))
        (die who "not a procedure" (if (procedure? complete) expire complete)))
      ((call/cc
         (lambda (escape)
           ;; For do-return, do-complete, do-expire, it is critical that there
           ;; be enough fuel before calling them and that reset-state be called
           ;; by them.  This ensures, if the fuel runs out while calling
           ;; stop-timer and when that continuation is later resumed, it will
           ;; continue on to calling the current do-whatever (which closes over
           ;; the current engine invocation continuation); if stop-timer was
           ;; done in do-whatever and the fuel ran out, when the continuation is
           ;; resumed, it would be resuming in a now old/dead do-whatever (which
           ;; closes over the previous engine invocation) and it would
           ;; incorrectly return to the previous engine invocation instead of
           ;; returning to the most recent one.  Calling stop-timer before
           ;; reset-state is also necessary to guarentee reset-state has enough
           ;; fuel to complete without running out of fuel.  Calling reset-state
           ;; from inside do-whatever is necessary so that the current
           ;; do-whatever still exists.
           (set! do-return
             (lambda (args)
               (reset-state)
               (escape (lambda () (apply values args)))))
           (set! do-complete
             (lambda (leftover vals)
               (reset-state)
               (escape (lambda () (apply complete leftover vals)))))
           (set! do-expire
             (lambda (resume-k)
               (reset-state)
               (escape (lambda () (expire (new-engine resume-k))))))
           (resume ticks)))))
    engine)
  
  (define (make-engine thunk)
    (unless (procedure? thunk)
      (die 'make-engine "not a procedure" thunk))    
    (new-engine
      (lambda (ticks)
        (let-values ((vals (begin (start-timer ticks) 
                                  (thunk)))) 
          (let ((leftover (stop-timer)))
            ;; stop-timer refills fuel, so there's enough for do-complete to reset-state
            (do-complete leftover vals))))))
  
  (define (engine-return . args) 
    ;; stop-timer refills fuel, so there's enough for do-return to reset-state
    (stop-timer)
    (do-return args))
  
  (define (mileage fuel thunk)
    (let loop ((eng (make-engine thunk)) (total-ticks 0))
      (eng fuel
        (lambda (ticks value)
          (+ total-ticks (- fuel ticks)))
        (lambda (new-eng)
          (loop new-eng (+ total-ticks fuel))))))
  
  (define (round-robin fuel engs)
    (if (null? engs)
      '()
      ((car engs) fuel
        (lambda (ticks value)
          (cons value (round-robin fuel (cdr engs))))
        (lambda (eng)
          (round-robin fuel
            (append (cdr engs) (list eng)))))))
  
  (define-syntax por
    (syntax-rules (fuel)
      ((_ (fuel f) x ...)
       (first-true f
         (list (make-engine (lambda () x)) ...)))))
  
  (define (first-true fuel engs)
    (if (null? engs)
      #F
      ((car engs) fuel
        (lambda (ticks value)
          (or value (first-true fuel (cdr engs))))
        (lambda (eng)
          (first-true fuel
            (append (cdr engs) (list eng)))))))
)
