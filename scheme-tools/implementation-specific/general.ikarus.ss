#!r6rs

(library

 (scheme-tools implementation-specific general)

 (export add1
         sub1
         assert
         command-line-arguments
         console-input-port
         environment
         eval
         exact->inexact
         format
         fprintf
         gensym
         inexact->exact
         interaction-environment
         make-parameter               
         modulo
         parameterize
         pretty-print
         system
         time
         void)
 
 (import (only (ikarus)
               add1
               sub1
               assert
               command-line-arguments
               console-input-port
               environment
               eval
               exact->inexact
               format
               fprintf
               gensym
               inexact->exact
               interaction-environment
               make-parameter               
               modulo
               parameterize
               pretty-print
               system
               time
               void))

 )