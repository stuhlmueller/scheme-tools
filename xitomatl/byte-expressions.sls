#!r6rs
;; Copyright 2010 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

#| A simple, flexible, general binary format with the benefits of S-Expressions.

<block>      ::=  <thing>*
<thing>      ::=  <datum> | <padding>
<datum>      ::=  <atom> | <list>
<atom>       ::=  <atom-tag> <byte>*
<atom-tag>   ::=  <var-bytes> not: #x00, #x01, #x02
<var-bytes>  ::=  <hi-byte>* <lo-byte>
<hi-byte>    ::=  <byte> with bit 7 set to 1  ; bits 0-6 usable
<lo-byte>    ::=  <byte> with bit 7 set to 0  ; bits 0-6 usable
<list>       ::=  #x01 <thing>* #x02  ; #x01 and #x02 are like ( and )
<padding>    ::=  #x00

- Allows the flexibility of arbitrary-sized datums and arbitrary amounts and
  structuring of them.

- Made for extensibility.  Users must give meaning to atoms, i.e. determine
  their length and structure.

- Can be used as an internal representation.  E.g., for memory-mapped files of
  loadable/linkable machine code with parts aligned as desired by using padding.

- Any things may be pointed to and the first byte(s) is(are) the tag.

- Can be navigated like a Lisp data structure, by holding references (pointers)
  to parts, testing types (tags), and destructuring parts.

- Atom tags may be arbitrary length, avoiding limitation, and are as compact as
  possible.

- Predicates for parts pointed to are simple:
  atom? : not #x00 nor #x01 nor #x02
  list? : #x01
  end-of-list? : #x02
  null? : #x01 #x02
  datum? : atom or list
  padding? : #x00

- Can be used as an external representation.

- Easily parsable (depending on complexity of user-defined atoms).

- Atom tags may represent immediate values without following bytes.

- Better than rigid-sized, rigid-structured, embedded offsets and pointers
  everywhere, tedious custom parsing, inflexible formats. |#

;; TODO?: Change #x00 to #x20 (space) and #x01 #x02 to #x28 #x29 (( )), for
;;        human readability and Emacs navigation?  It would make choosing atom
;;        tags less simple...

;; TODO?: Change format to have pair delimiters, so lists can be navigated like
;;        Scheme and so pairs exist?  What are the consequences..?

;; TODO: Procedures for working with BE-lists.  length, next, etc.

(library (xitomatl byte-expressions)
  (export
    bytevector->varbytes
    varbytes->bytevector
    bytevector->uint
    get-sized-bytes
    reader)
  (import
    (rnrs))

  ;;;; variable-length byte sequences

  (define (u8-list->bitlist hi l)
    (define (bitlist x)
      (do ((i 0 (+ 1 i))
           (a '() (cons (if (fxbit-set? x i) 1 0) a)))
          ((<= hi i) a)))
    (let loop ((l (reverse l)) (a '()))
      (if (null? l)
        (apply append a)
        (loop (cdr l) (cons (bitlist (car l)) a)))))

  (define (bitlist->bytevector hi n-u8 l)
    (let loop ((l (reverse l)) (i 0) (u8 0) (a '()))
      (if (null? l)
        (u8-list->bytevector (if (< 0 i) (cons u8 a) a))
        (if (<= hi i)
          (loop l 0 n-u8 (cons u8 a))
          (loop (cdr l) (+ 1 i) (fxcopy-bit u8 i (car l)) a)))))

  (define (drop m x)
    (let loop ((n (mod (length x) m)) (x x))
      (if (and (< 0 n) (= 0 (car x)))
        (loop (- n 1) (cdr x))
        x)))

  (define (bytevector->varbytes bv bop)
    (put-bytevector bop
      (bitlist->bytevector 7 #b10000000
        (let* ((x (bytevector->u8-list bv))
               (y (u8-list->bitlist 8 x)))
          (if (and (= 0 (car x)) (pair? (cdr x))) y (drop 7 y))))))

  (define (varbytes->bytevector bip)
    (let loop ((a '()))
      (let ((u8 (get-u8 bip)))
        (if (fxbit-set? u8 7)
          (loop (cons u8 a))
          (bitlist->bytevector 8 0
            (let* ((x (reverse (cons u8 a)))
                   (y (u8-list->bitlist 7 x)))
              (if (= 0 (car x)) y (drop 8 y))))))))

  (define (bytevector->uint bv)
    (bytevector-uint-ref bv 0 'big (bytevector-length bv)))

  (define (get-sized-bytes bip)
    (get-bytevector-n bip
      (bytevector->uint (varbytes->bytevector bip))))

  ;;;; raw

  (define (BE-atom? bip) (< 2 (lookahead-u8 bip)))

  (define (BE-list? bip) (= #x01 (lookahead-u8 bip)))

  (define (BE-list-end? bip) (= #x02 (lookahead-u8 bip)))

  (define (BE-datum? bip) (or (BE-atom? bip) (BE-list? bip)))

  (define (BE-padding? bip) (= #x00 (lookahead-u8 bip)))

  ;; TODO: more -- remember lookahead vs. moving

  ;;;; parsed

  (define (reader atom-helper)
    (lambda (bip)
      (define (oops msg)
        (raise (condition (make-i/o-read-error)
                          (make-who-condition 'byte-expression-reader)
                          (make-message-condition msg)
                          (make-i/o-port-error bip))))
      (let recur ((a #F))
        (if (port-eof? bip)
          (if a
            (oops "EOF inside list")
            (eof-object))
          (let ((tag (varbytes->bytevector bip)))
            (cond ((bytevector=? #vu8(#x02) tag) ; list end
                   (if a
                     (reverse a)
                     (oops "unexpected list end")))
                  ((bytevector=? #vu8(#x01) tag) ; list begin
                   (let ((x (recur '())))
                     (if a
                       (recur (cons x a))
                       x)))
                  ((bytevector=? #vu8(#x00) tag) ; padding
                   (recur a))
                  (else ; atom
                   (let ((x (atom-helper tag bip)))
                     (if a
                       (recur (cons x a))
                       x)))))))))

  ;; TODO: writer, with delegation for non-lists.
)
