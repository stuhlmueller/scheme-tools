#!r6rs

(library

 (scheme-tools implementation-specific foreign)

 (export free pointer-ref-c-float pointer-ref-c-unsigned-short
         pointer-ref-c-signed-int dlerror pointer->integer
         pointer-ref-c-double pointer-set-c-pointer!
         pointer-ref-c-unsigned-int pointer-size
         pointer-ref-c-signed-long pointer-set-c-float!
         make-c-callback pointer-ref-c-pointer pointer?
         pointer-set-c-double! pointer-ref-c-unsigned-long
         pointer-set-c-long-long! pointer-ref-c-signed-char
         pointer-ref-c-signed-long-long memcpy make-c-callout errno
         pointer-set-c-short! pointer-set-c-int!
         pointer-set-c-char! pointer-set-c-long! dlopen
         pointer-ref-c-unsigned-long-long integer->pointer
         pointer-ref-c-unsigned-char dlsym
         pointer-ref-c-signed-short dlclose malloc)

 (import (ikarus foreign))

 )