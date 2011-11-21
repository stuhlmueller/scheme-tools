#!r6rs

(library

 (scheme-tools macros)

 (export opt-timeit
         nth-value)

 (import (rnrs)
         (scheme-tools srfi-compat :1)
         (scheme-tools readable-scheme)
         (scheme-tools implementation-specific))

 (define-syntax opt-timeit
   (syntax-rules ()
     [(opt-timeit test expr) (if test (time expr) expr)]))

 (define-syntax nth-value
   (syntax-rules ()
     ((nth-value n values-producing-form)
      (call-with-values
          (lambda () values-producing-form)
        (lambda all-values
          (list-ref all-values n))))))

 )