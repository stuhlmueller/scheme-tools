#!r6rs
;; Copyright 2010 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

;; TODO?: SXML format-mode

;; TODO?: Colored output.  Detect TERM.  Use IRREGEX_TOOL_COLORS.

;; TODO: Detect binary-type files and have option to skip.

(import
  (rnrs)
  (srfi :39 parameters)
  (xitomatl match)
  (xitomatl common)
  (xitomatl enumerators)
  (xitomatl define)
  (xitomatl irregex)
  (xitomatl irregex counting)
  (xitomatl irregex-tool))

(define format-mode (make-parameter 's-expr))

(define port-chunk-size (make-parameter 1024))  ;; good size?

(define interactive
  (let ((prompt (lambda (str)
                  (display str)
                  (flush-output-port (current-output-port))
                  (get-line (current-input-port)))))
    (case-lambda
      (()
       (let ((irx (prompt "\nEnter regex: ")))
         (newline)
         (unless (eof-object? irx)
           (interactive irx))))
      ((irx)
       (set! irx (irregex irx 'fast))
       (let loop ()
         (let ((line (prompt "Enter line: ")))
           (if (eof-object? line)
             (newline)
             (let ((m (irregex-search irx line)))
               (if m
                 (let ((max (irregex-match-num-submatches m)))
                   (let show ((n 0))
                     (when (<= n max)
                       (printf "~a:\t~s\n" n (irregex-match-substring m n))
                       (show (+ 1 n)))))
                 (display "No match.\n"))
               (loop)))))))))

(define (lines irx . files/dirs)
  (define (print-start/s-expr filename)
    (printf "(~s\n" filename))
  (define (print-end/s-expr _)
    (display ")\n"))
  (define (print-match/s-expr line line-num m)
    (define max (irregex-match-num-submatches m))
    (printf " (~s ~s" line-num line)
    (let loop ((n 0))
      (if (<= n max)
        (begin (printf "\n  ~s" (irregex-match-substring m n))
               (loop (+ 1 n)))
        (printf ")\n"))))
  (define-values (print-start print-end print-match)
    (case (format-mode)
      ((s-expr)
       (values print-start/s-expr print-end/s-expr print-match/s-expr))))
  (let ((last-file
         (fold/enumerator
          (lines-enumerator irx)
          files/dirs
          (lambda (file line-num line match-obj last-file)
            (unless (equal? file last-file)
              (when last-file
                (print-end last-file))
              (print-start file))
            (print-match line line-num match-obj)
            (values #T file))
          #F)))
    (when last-file
      (print-end last-file))))

(define (single irx . files/dirs)
  (define (print-start/s-expr filename)
    (printf "(~s\n" filename))
  (define (print-end/s-expr _)
    (display ")\n"))
  (define (print-match/s-expr m)
    (define (p fmt n)
      (let-values ((s (counted-match-start-positions m n))
                   (e (counted-match-end-positions m n)))
        (printf fmt (irregex-match-substring m n) s e)))
    (define max (irregex-match-num-submatches m))
    (let loop ((n 0))
      (cond ((= n 0)
             (p " ((~s\n   ~s\n   ~s)" n)
             (loop (+ 1 n)))
            ((<= n max)
             (p "\n  (~s\n   ~s\n   ~s)" n)
             (loop (+ 1 n)))
            (else
             (printf ")\n")))))
  (define-values (print-start print-end print-match)
    (case (format-mode)
      ((s-expr)
       (values print-start/s-expr print-end/s-expr print-match/s-expr))))
  (let ((last-file
         (fold/enumerator
          (single-enumerator irx (port-chunk-size))
          files/dirs
          (lambda (file match-obj last-file)
            (unless (equal? file last-file)
              (when last-file
                (print-end last-file))
              (print-start file))
            (print-match match-obj)
            (values #T file))
          #F)))
    (when last-file
      (print-end last-file))))

(define (print-help/exit)
  (define d display)
  (printf "Usage: ~a [command [options ...]]\n" (car (command-line)))
  (d " Commands:\n")
  (d "  (-i) --interactive [regex]       Prompt for lines to match against regex.\n")
  (d "                                    Prompt for regex if not supplied.\n")
  (d "  (-l) --lines regex [paths ...]   Search files, recursively descending into\n")
  (d "                                    directories, or (current-input-port),\n")
  (d "                                    for lines containing a match for regex.\n")
  (d "  (-s) --single regex [paths ...]  Search files, recursively descending into\n")
  (d "                                    directories, or (current-input-port),\n")
  (d "                                    across lines, with . matching newline.\n")
  (d " If no command is supplied, interactive is used.\n")
  (exit #F))

(define main
  (match-lambda
    ((_)
     (main '(#F "--interactive")))
    ((_ (:or "--interactive" "-i") args (... 0 1))
     (apply interactive args))
    ((_ (:or "--lines" "-l") regex . args)
     (apply lines regex args))
    ((_ (:or "--single" "-s") regex . args)
     (apply single regex args))
    #;((_ (:or "replace" "r") regex replacement . args)
     (apply replace args))
    (_
     (print-help/exit))))

(main (command-line))
