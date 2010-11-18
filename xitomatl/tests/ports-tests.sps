#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

;; NOTE: The compound-input-port uses of coroutines are known to fail on PLT Scheme.

(import
  (rnrs)
  (xitomatl ports)
  (srfi :78 lightweight-testing)
  (xitomatl coroutines)
  (xitomatl bytevectors)
  (xitomatl control)
  (only (xitomatl file-system base) current-directory))

(define-syntax check-AV-msg
  (syntax-rules ()
    ((_ msg expr)
     (check (guard (ex (else (and (assertion-violation? ex)
                                  (message-condition? ex)
                                  (condition-message ex))))
              expr
              'unexpected-return)
            => msg))))

(define-syntax check-ex
  (syntax-rules ()
    ((_ expr)
     (check (guard (ex (else 'caught))
              expr
              'unexpected-return)
            => 'caught))))


;;;; Predicates

;; binary-input-port? binary-output-port?
;; textual-input-port? textual-output-port? port-closed?
(define bip (open-bytevector-input-port #vu8(1 2 3)))
(check (binary-input-port? bip) => #T)
(check (binary-output-port? bip) => #F)
(check (textual-input-port? bip) => #F)
(check (textual-output-port? bip) => #F)
(check (port-closed? bip) => #F)
(close-port bip)
(check (port-closed? bip) => #T)
(define bop
  (let-values (((bop bop-g) (open-bytevector-output-port)))
    bop))
(check (binary-input-port? bop) => #F)
(check (binary-output-port? bop) => #T)
(check (textual-input-port? bop) => #F)
(check (textual-output-port? bop) => #F)
(check (port-closed? bop) => #F)
(close-port bop)
(check (port-closed? bop) => #T)
(define sip (open-string-input-port "abc"))
(check (binary-input-port? sip) => #F)
(check (binary-output-port? sip) => #F)
(check (textual-input-port? sip) => #T)
(check (textual-output-port? sip) => #F)
(check (port-closed? sip) => #F)
(close-port sip)
(check (port-closed? sip) => #T)
(define sop
  (let-values (((sop sop-g) (open-string-output-port)))
    sop))
(check (binary-input-port? sop) => #F)
(check (binary-output-port? sop) => #F)
(check (textual-input-port? sop) => #F)
(check (textual-output-port? sop) => #T)
(check (port-closed? sop) => #F)
(close-port sop)
(check (port-closed? sop) => #T)

;;;; Getting everything from a port

(define text
"Blah 123 ()\n\
\"another\" #\\l #(i n) #\\e\n\
a n d ((()))")

(let ((x 0))
  (call-with-port (open-string-input-port text)
    (lambda (sip)
      (port-for-each 
        (lambda (c) (set! x (+ 1 x))) 
        get-char 
        sip)))
  (check x => 49))
(check (call-with-port (open-string-input-port text)
         (lambda (sip)
           (port-map values get-char sip)))
       => '(#\B #\l #\a #\h #\space #\1 #\2 #\3 #\space #\( #\)
            #\linefeed #\" #\a #\n #\o #\t #\h #\e #\r #\" #\space #\#
            #\\ #\l #\space #\# #\( #\i #\space #\n #\) #\space #\#
            #\\ #\e #\linefeed #\a #\space #\n #\space #\d #\space #\(
            #\( #\( #\) #\) #\)))
(check (call-with-port (open-string-input-port text)
         get-lines-all)
       => '("Blah 123 ()" "\"another\" #\\l #(i n) #\\e" "a n d ((()))"))
(check (call-with-port (open-string-input-port text)
         read-all)
       => '(Blah 123 () "another" #\l #(i n) #\e a n d ((()))))

;;;; Compound input ports

;;;; compound binary

(define b0-bv #vu8(0 1 2 3 4 5 6 7 8 9 10 12 13 14 15 16 17 18 19))
(define b1-bv #vu8())
;; The implementation's native-transcoder might error on these two, but
;; most implementations use UTF-8 and (error-handling-mode replace)
(define b2-bv #vu8(255 254))
(define b3-bv #vu8(117 148 234 185 168 143 106 207 80 37 22 104 168 54 242 159 225 21 243 209 9 131 246 11 64 107 33 166 203 175 186 50 70 17 147 206 242 149 60 193 211 110 86 208 53 151 120 194 188 70 178 97 113 119 177))
(define b4-bv #vu8(1))
(define b5-bv #vu8(84 104 101 32 102 117 110 100 97 109 101 110 116 97 108 32 110 97 116 117 114 101 32 111 102 32 114 101 97 108 105 116 121 32 105 115 58 32 102 114 97 99 116 97 108 32 111 102 32 110 111 118 101 108 116 121 46))
;; write temp files
(define b0-fn "/tmp/b0")
(define b1-fn "/tmp/b1")
(define b2-fn "/tmp/b2")
(define b3-fn "/tmp/b3")
(define b4-fn "/tmp/b4")
(define b5-fn "/tmp/b5")
(for-each
  (lambda (fn b-bv)
    (call-with-port (open-file-output-port fn)
      (lambda (bp)
        (put-bytevector bp b-bv))))
  (list b0-fn b1-fn b2-fn b3-fn b4-fn b5-fn)
  (list b0-bv b1-bv b2-bv b3-bv b4-bv b5-bv))
;;;; errors
(check-AV-msg "not a list or procedure"
  (open-binary-compound-input-port 'oops))
(check-AV-msg "not a proper list"
  (call-with-port 
      (open-binary-compound-input-port (cons #vu8() 'oops))
    get-u8))
(check-AV-msg "Invalid value in supplied list. Not a binary input port or bytevector."
  (call-with-port 
      (open-binary-compound-input-port (list #vu8() 'oops))
    get-u8))
(check-AV-msg "Invalid value returned from supplied procedure. Not a binary input port or bytevector."
  (call-with-port 
      (open-binary-compound-input-port (lambda () 'oops))
    get-u8))
;;;; initially empty
(let ((bcip (open-binary-compound-input-port '())))
  (check (eof-object? (get-u8 bcip)) => #T)
  (close-port bcip)
  (check-ex (get-u8 bcip)))
(let ((bcip (open-binary-compound-input-port (lambda () #F))))
  (check (eof-object? (get-u8 bcip)) => #T)
  (close-port bcip)
  (check-ex (get-u8 bcip)))
;;;; single component
(check (call-with-port
           (open-binary-compound-input-port (list b0-bv))
         get-bytevector-all)
       => b0-bv)
(check (eof-object?
        (call-with-port
            (open-binary-compound-input-port 
              (coroutine ()
                (yield b1-bv)
                (yield #F)))
          get-bytevector-all))
       => #T)
(call-with-port (open-file-input-port b2-fn)
  (lambda (p)
    (check (call-with-port
               (open-binary-compound-input-port (list p))
             get-bytevector-all)
           => b2-bv)))
(call-with-port (open-file-input-port b3-fn)
  (lambda (p)
    (check (call-with-port
               (open-binary-compound-input-port 
                 (let ((v p)) 
                   (lambda ()
                     (begin0
                       v
                       (set! v #F)))))
             get-bytevector-all)
           => b3-bv)))
;;;; two components
(check (call-with-port
           (open-binary-compound-input-port (list b4-bv b5-bv))
         get-bytevector-all)
       => (bytevector-append b4-bv b5-bv))
(check (call-with-port
           (open-binary-compound-input-port 
             (coroutine ()
               (yield b1-bv)
               (yield b0-bv)
               (yield #F)))
         get-bytevector-all)
       => (bytevector-append b1-bv b0-bv))
(call-with-port (open-file-input-port b2-fn)
  (lambda (pa)
    (call-with-port (open-file-input-port b3-fn)
      (lambda (pb)
        (check (call-with-port
                   (open-binary-compound-input-port (list pa pb))
                 get-bytevector-all)
               => (bytevector-append b2-bv b3-bv))))))
(call-with-port (open-file-input-port b3-fn)
  (lambda (pa)
    (call-with-port (open-file-input-port b4-fn)
      (lambda (pb)
        (check (call-with-port
                   (open-binary-compound-input-port 
                    (let ((v (list pa pb #F)))
                      (lambda ()
                        (begin0
                          (car v)
                          (set! v (cdr v))))))
                 get-bytevector-all)
               => (bytevector-append b3-bv b4-bv))))))
;;;; many components
(check (call-with-port
           (open-binary-compound-input-port (list b0-bv b1-bv b2-bv b3-bv b4-bv b5-bv))
         get-bytevector-all)
       => (bytevector-append b0-bv b1-bv b2-bv b3-bv b4-bv b5-bv))
(check (call-with-port
           (open-binary-compound-input-port 
             (coroutine ()
               (yield b1-bv)
               (yield b0-bv)
               (yield b0-bv)
               (yield b1-bv)
               (yield b4-bv)
               (yield b3-bv)
               (yield b2-bv)
               (yield #F)))
         get-bytevector-all)
       => (bytevector-append b1-bv b0-bv b0-bv b1-bv b4-bv b3-bv b2-bv))
(let ((ps (map open-file-input-port 
               (list b4-fn b2-fn b3-fn b1-fn b0-fn b5-fn b5-fn b3-fn))))
  (check (call-with-port
             (open-binary-compound-input-port ps)
           get-bytevector-all)
         => (bytevector-append b4-bv b2-bv b3-bv b1-bv b0-bv b5-bv b5-bv b3-bv))
  (for-each close-port ps))
(let ((ps (map open-file-input-port 
               (list b3-fn b3-fn b5-fn b1-fn b0-fn b3-fn b4-fn b2-fn))))
  (check (call-with-port
             (open-binary-compound-input-port
              (let ((v (append ps '(#F))))
                (lambda ()
                  (begin0 
                    (car v)
                    (set! v (cdr v))))))
           get-bytevector-all)
         => (bytevector-append b3-bv b3-bv b5-bv b1-bv b0-bv b3-bv b4-bv b2-bv))
  (for-each close-port ps))
;;;; mixed bytevectors and binary input ports
(let ((pa (open-file-input-port b5-fn))
      (pb (open-file-input-port b0-fn))
      (pc (open-file-input-port b2-fn)))
  (check (call-with-port
             (open-binary-compound-input-port (list pa b1-bv pb pc b4-bv b5-bv))
           get-bytevector-all)
         => (bytevector-append b5-bv b1-bv b0-bv b2-bv b4-bv b5-bv))
  (for-each close-port (list pa pb pc)))
(let ((pa (open-file-input-port b1-fn))
      (pb (open-file-input-port b2-fn))
      (pc (open-file-input-port b3-fn)))
  (check (call-with-port
             (open-binary-compound-input-port 
              (coroutine ()
                (yield b1-bv)
                (yield b0-bv)
                (yield pa)
                (yield b1-bv)
                (yield pb)
                (yield b3-bv)
                (yield pc)
                (yield #F)))
           get-bytevector-all)
         => (bytevector-append b1-bv b0-bv b1-bv b1-bv b2-bv b3-bv b3-bv))
  (for-each close-port (list pa pb pc)))
;;;; closing before finished closes all component ports
(let ((pa (open-file-input-port b5-fn))
      (pb (open-file-input-port b0-fn))
      (pc (open-file-input-port b2-fn))
      (pd (open-file-input-port b3-fn)))
  (check (call-with-port
             (open-binary-compound-input-port 
               (coroutine () 
                 (for-each yield (list pa b1-bv pb pc b4-bv b5-bv pd #F))))
           (lambda (bcip)
             (begin0
               (get-u8 bcip)
               (close-port bcip))))
         => 84)
  (check-ex (get-u8 pa))
  (check-ex (get-u8 pb))
  (check-ex (get-u8 pc))
  (check-ex (get-u8 pd)))

;;;; compound textual (using native-transcoder)

(define t0-str "abcdefghijk\xD6D5;\xD64F;\xD681;lmnopqrstuvwxyz   ")
(define t1-str "AB")
(define t2-str "")
(define t3-str "1")
(define t4-str "q0934utasdklnq\xafff;ow4utwλeuakbfFq832y4r89aosidfujLAHFAIWHifgshkHiag8ryhkuhg hgku D KUDgh i4t8gq\x3456;\x599;34ti23lf zdsk ghae\xD689;\xD6D2;oirigh askdjfh λ q8gih aksjd h")
(define t5-str "")
(define (bv->str bv)
  (bytevector->string bv (native-transcoder)))
(define b0-str (bv->str b0-bv))
(define b1-str (bv->str b1-bv))
(define b2-str (bv->str b2-bv))
(define b3-str (bv->str b3-bv))
(define b4-str (bv->str b4-bv))
(define b5-str (bv->str b5-bv))
;; write temp files
(define t0-fn "/tmp/t0")
(define t1-fn "/tmp/t1")
(define t2-fn "/tmp/t2")
(define t3-fn "/tmp/t3")
(define t4-fn "/tmp/t4")
(define t5-fn "/tmp/t5")
(for-each
  (lambda (fn t-str)
    (call-with-output-file fn
      (lambda (tp)
        (put-string tp t-str))))
  (list t0-fn  t1-fn  t2-fn  t3-fn  t4-fn  t5-fn)
  (list t0-str t1-str t2-str t3-str t4-str t5-str))
;;;; errors
(check-AV-msg "not a list or procedure"
  (open-textual-compound-input-port 'oops))
(check-AV-msg "not a proper list"
  (call-with-port 
      (open-textual-compound-input-port (cons "" 'oops))
    get-char))
(check-AV-msg "Invalid value in supplied list. Not an input port or string or bytevector."
  (call-with-port 
      (open-textual-compound-input-port (list "" 'oops))
    get-char))
(check-AV-msg "Invalid value returned from supplied procedure. Not an input port or string or bytevector."
  (call-with-port 
      (open-textual-compound-input-port (lambda () 'oops))
    get-char))
;;;; initially empty
(let ((tcip (open-textual-compound-input-port '())))
  (check (eof-object? (get-char tcip)) => #T)
  (close-port tcip)
  (check-ex (get-char tcip)))
(let ((tcip (open-textual-compound-input-port (lambda () #F))))
  (check (eof-object? (get-char tcip)) => #T)
  (close-port tcip)
  (check-ex (get-char tcip)))
;;;; single component
(check (call-with-port
           (open-textual-compound-input-port (list t0-str))
         get-string-all)
       => t0-str)
(check (call-with-port
           (open-textual-compound-input-port (list b0-bv))
         get-string-all)
       => b0-str)
(check (eof-object?
        (call-with-port
            (open-textual-compound-input-port 
              (coroutine ()
                (yield t2-str)
                (yield #F)))
          get-string-all))
       => #T)
(check (eof-object?
        (call-with-port
            (open-textual-compound-input-port 
              (coroutine ()
                (yield b1-bv)
                (yield #F)))
          get-string-all))
       => #T)
(call-with-port (open-input-file t2-fn)
  (lambda (p)
    (check (eof-object?
            (call-with-port
                (open-textual-compound-input-port (list p))
              get-string-all))
           => #T)))
(call-with-port (open-file-input-port b2-fn)
  (lambda (p)
    (check (call-with-port
               (open-textual-compound-input-port (list p))
             get-string-all)
           => b2-str)))
(call-with-port (open-input-file t3-fn)
  (lambda (p)
    (check (call-with-port
               (open-textual-compound-input-port 
                 (let ((v p)) 
                   (lambda ()
                     (begin0
                       v
                       (set! v #F)))))
             get-string-all)
           => t3-str)))
(call-with-port (open-file-input-port b3-fn)
  (lambda (p)
    (check (call-with-port
               (open-textual-compound-input-port 
                 (let ((v p)) 
                   (lambda ()
                     (begin0
                       v
                       (set! v #F)))))
             get-string-all)
           => b3-str)))
;;;; two components
(check (call-with-port
           (open-textual-compound-input-port (list t4-str b5-bv))
         get-string-all)
       => (string-append t4-str b5-str))
(check (call-with-port
           (open-textual-compound-input-port (list b4-bv t5-str))
         get-string-all)
       => (string-append b4-str t5-str))
(check (call-with-port
           (open-textual-compound-input-port 
             (coroutine ()
               (yield t1-str)
               (yield b0-bv)
               (yield #F)))
         get-string-all)
       => (string-append t1-str b0-str))
(check (call-with-port
           (open-textual-compound-input-port 
             (coroutine ()
               (yield b1-bv)
               (yield t0-str)
               (yield #F)))
         get-string-all)
       => (string-append b1-str t0-str))
(call-with-port (open-input-file t2-fn)
  (lambda (pa)
    (call-with-port (open-file-input-port b3-fn)
      (lambda (pb)
        (check (call-with-port
                   (open-textual-compound-input-port (list pa pb))
                 get-string-all)
               => (string-append t2-str b3-str))))))
(call-with-port (open-file-input-port b2-fn)
  (lambda (pa)
    (call-with-port (open-input-file t3-fn)
      (lambda (pb)
        (check (call-with-port
                   (open-textual-compound-input-port (list pa pb))
                 get-string-all)
               => (string-append b2-str t3-str))))))
(call-with-port (open-input-file t3-fn)
  (lambda (pa)
    (call-with-port (open-file-input-port b4-fn)
      (lambda (pb)
        (check (call-with-port
                   (open-textual-compound-input-port 
                    (let ((v (list pa pb #F)))
                      (lambda ()
                        (begin0
                          (car v)
                          (set! v (cdr v))))))
                 get-string-all)
               => (string-append t3-str b4-str))))))
(call-with-port (open-file-input-port b3-fn)
  (lambda (pa)
    (call-with-port (open-input-file t4-fn)
      (lambda (pb)
        (check (call-with-port
                   (open-textual-compound-input-port 
                    (let ((v (list pa pb #F)))
                      (lambda ()
                        (begin0
                          (car v)
                          (set! v (cdr v))))))
                 get-string-all)
               => (string-append b3-str t4-str))))))
;;;; many components
(check (call-with-port
           (open-textual-compound-input-port (list b0-bv t1-str t2-str t3-str b4-bv b5-bv))
         get-string-all)
       => (string-append b0-str t1-str t2-str t3-str b4-str b5-str))
(check (call-with-port
           (open-textual-compound-input-port 
             (coroutine ()
               (yield t1-str)
               (yield b0-bv)
               (yield t0-str)
               (yield b1-bv)
               (yield b4-bv)
               (yield b3-bv)
               (yield t2-str)
               (yield #F)))
         get-string-all)
       => (string-append t1-str b0-str t0-str b1-str b4-str b3-str t2-str))
(let ((ps (map (lambda (fn t)
                 ((if t open-input-file open-file-input-port) fn)) 
               (list t4-fn t2-fn b3-fn t1-fn b0-fn b5-fn t5-fn b3-fn)
               '(    #T    #T    #F    #T    #F    #F    #T    #F))))
  (check (call-with-port
             (open-textual-compound-input-port ps)
           get-string-all)
         => (string-append t4-str t2-str b3-str t1-str b0-str b5-str t5-str b3-str))
  (for-each close-port ps))
(let ((ps (map (lambda (fn t)
                 ((if t open-input-file open-file-input-port) fn)) 
               (list b3-fn b3-fn t5-fn b1-fn b0-fn t3-fn t4-fn t2-fn)
               '(    #F    #F    #T    #F    #F    #T    #T    #T))))
  (check (call-with-port
             (open-textual-compound-input-port
              (let ((v (append ps '(#F))))
                (lambda ()
                  (begin0 
                    (car v)
                    (set! v (cdr v))))))
           get-string-all)
         => (string-append b3-str b3-str t5-str b1-str b0-str t3-str t4-str t2-str))
  (for-each close-port ps))
;;;; mixed strings, bytevectors, textual input ports, and binary input ports
(let ((pa (open-input-file t5-fn))
      (pb (open-file-input-port b0-fn))
      (pc (open-input-file t2-fn)))
  (check (call-with-port
             (open-textual-compound-input-port (list pa b1-bv pb pc t4-str b5-bv))
           get-string-all)
         => (string-append t5-str b1-str b0-str t2-str t4-str b5-str))
  (for-each close-port (list pa pb pc)))
(let ((pa (open-file-input-port b1-fn))
      (pb (open-input-file t2-fn))
      (pc (open-file-input-port b3-fn)))
  (check (call-with-port
             (open-textual-compound-input-port 
              (coroutine ()
                (yield b1-bv)
                (yield b0-bv)
                (yield pa)
                (yield t1-str)
                (yield pb)
                (yield t3-str)
                (yield pc)
                (yield #F)))
           get-string-all)
         => (string-append b1-str b0-str b1-str t1-str t2-str t3-str b3-str))
  (for-each close-port (list pa pb pc)))
;;;; closing before finished closes all component ports
(let ((pa (open-input-file t5-fn))
      (pb (open-file-input-port b0-fn))
      (pc (open-input-file t2-fn))
      (pd (open-file-input-port b3-fn)))
  (check (call-with-port
             (open-textual-compound-input-port 
               (coroutine () 
                 (for-each yield (list pa b1-bv pb pc t4-str t5-str pd #F))))
           (lambda (tcip)
             (begin0
               (get-char tcip)
               (close-port tcip))))
         => #\x0)
  (check-ex (get-char pa))
  (check-ex (get-char pb))
  (check-ex (get-char pc))
  (check-ex (get-char pd)))

;;;; clean-up
(current-directory "/tmp")
(for-each delete-file '("b0" "b1" "b2" "b3" "b4" "b5" "t0" "t1" "t2" "t3" "t4" "t5"))

(check-report)
