#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl profiler srfi-time)
  (export
    case-lambda/profiled lambda/profiled define/profiled
    generate-report print-report)
  (import
    (rnrs)
    (srfi :19 time)
    (xitomatl profiler meta)
    (only (xitomatl enumerators) fold)
    (only (xitomatl common) fprintf pretty-print format))
  
  (define make-profiled-proxy 
    (make-make-profiled-proxy current-time add-duration 
                              (lambda (x y)
                                (if (eq? 'time-duration (time-type y))
                                  (subtract-duration x y)
                                  (time-difference x y)))))
  
  (def--/profiled case-lambda/profiled
                  lambda/profiled
                  define/profiled
                  make-profiled-proxy)
  
  (define (generate-report)
    (let-values (((keys vals) (hashtable-entries (profiled-procedures-HT))))
      (vector->list vals)))
  
  (define print-report
    (case-lambda
      (()
       (print-report #F))
      ((print-unused) 
       (print-report print-unused (generate-report)))
      ((print-unused report) 
       (print-report print-unused report (current-output-port)))
      ((print-unused report port)
       (define (fpf str . args) (apply fprintf port str args))
       (define (fpp x) (pretty-print x port))
       (define (fmt-num-set ns)
         (apply string-append (map (lambda (n) (format " ~a" n)) (list-sort < ns))))
       (for-each
        (lambda (pp)           
          (let-values 
              (((calls-num returns-num entries/exits-num args-nums vals-nums times)
                (fold 
                 (profiled-procedure-uses pp)
                 (lambda (u c r e a v t)
                   (let ((start (procedure-use-start u))
                         (stop (procedure-use-stop u))
                         (called (procedure-use-called u))
                         (returned (procedure-use-returned u)))
                     (values #T 
                             (if called (+ 1 c) c)
                             (if returned (+ 1 r) r)
                             (+ 1 e)
                             (if (and called (not (memv called a)))
                               (cons called a)
                               a)
                             (if (and returned (not (memv returned v)))
                               (cons returned v)
                               v)
                             (let* ((d (time-difference stop start))
                                    (s (let ((s (time-second d))
                                             (ns (/ (time-nanosecond d) #e1e9)))
                                         ((if (negative? s) - +) s ns))))
                               (cons (max s 0) t)))))
                 0 0 0 '() '() '())))
            (when (or (positive? entries/exits-num) print-unused)
              (fpf "\n=================================================================\n")
              (fpf "Profile for:\n")
              (fpp (profiled-procedure-source-code pp))
              (fpf "Statistics:\n")
              (fpf " calls: ~s   returns: ~s   entries/exits: ~s\n" 
                   calls-num returns-num entries/exits-num)
              (unless (null? args-nums)
                (fpf " numbers of arguments to calls:~a\n" (fmt-num-set args-nums)))
              (unless (null? vals-nums)
                (fpf " numbers of values returned:~a\n" (fmt-num-set vals-nums)))
              (unless (null? times)
                (let-values (((count total minimum maximum)
                              (fold times
                                    (lambda (ti c to mi ma)
                                      (values #T 
                                              (+ 1 c)
                                              (+ ti to)
                                              (if mi (min ti mi) ti)
                                              (max ti ma)))
                                    0 0 #F 0)))
                  (fpf "   total time: ~s sec\n" (inexact total))
                  (fpf " average time: ~s sec\n" (inexact (/ total count)))
                  (fpf " minimum time: ~s sec\n" (inexact minimum))
                  (fpf " maximum time: ~s sec\n" (inexact maximum))))
              (fpf "=================================================================\n"))))
        report))))

)
