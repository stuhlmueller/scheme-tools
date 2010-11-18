#!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl fmt c (0 7))
  (export
    fmt-in-macro? fmt-expression? fmt-return? fmt-default-type
    fmt-newline-before-brace? fmt-braceless-bodies?
    fmt-indent-space fmt-switch-indent-space fmt-op fmt-gen
    c-in-expr c-in-stmt c-in-test
    c-paren c-maybe-paren c-type c-literal? c-literal char->c-char
    c-struct c-union c-class c-enum c-attribute c-typedef c-cast
    c-expr c-expr/sexp c-apply c-op c-indent c-current-indent-string
    c-wrap-stmt c-open-brace c-close-brace
    c-block c-braced-block c-begin
    c-fun c-var c-prototype c-param c-param-list
    c-while c-for c-if c-switch
    c-case c-case/fallthrough c-default
    c-break c-continue c-return c-goto c-label
    c-static c-const c-extern c-volatile c-auto c-restrict c-inline
    c++ c-- c+ c- c* c/ c% c& c^ c~ c! c&& c<< c>> c== c!=
    c< c> c<= c>= c= c+= c-= c*= c/= c%= c&= c^= c<<= c>>=
    c++/post c--/post c. c->
    c-bit-or c-or c-bit-or=
    cpp-if cpp-ifdef cpp-ifndef cpp-elif cpp-endif cpp-undef
    cpp-include cpp-define cpp-wrap-header cpp-pragma cpp-line
    cpp-error cpp-warning cpp-stringify cpp-sym-cat
    c-comment c-block-comment)
  (import
    (rename (rnrs) (for-all every))
    (only (srfi :13 strings) substring/shared string-index)
    (srfi :23 error tricks)
    (xitomatl include)
    (xitomatl fmt base (0 7)))

  (SRFI-23-error->R6RS "(library (xitomatl fmt c (0 7)))"
   (include/resolve ("xitomatl" "fmt") "fmt-c.scm"))
)
