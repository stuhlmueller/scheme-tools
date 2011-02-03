#!r6rs

(library

 (scheme-tools implementation-specific general)

 (export assert
         console-input-port
         environment
         eval
         exact->inexact
         format
         fprintf
         gensym
         inexact->exact
         make-parameter               
         modulo
         parameterize
         pretty-print
         system
         void)

 (import (only (ikarus)
               assert
               console-input-port
               environment
               eval
               exact->inexact
               format
               fprintf
               gensym
               inexact->exact
               make-parameter               
               modulo
               parameterize
               pretty-print
               system
               void))

 )