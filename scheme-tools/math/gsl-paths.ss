#!r6rs

(library

 (scheme-tools math gsl-paths)
 
 (export libgsl-paths libgslcblas-paths find-lib)

 (import (rnrs))
 
 (define libgslcblas-paths
   (list
    "libgslcblas"
    "/opt/local/lib/libgslcblas.dylib"
    "/sw/lib/libgslcblas.dylib"
    "/usr/local/lib/libgslcblas.dylib"
    "/opt/local/var/macports/software/gsl/1.12_0/opt/local/lib/libgslcblas.dylib"
    "/opt/local/var/macports/software/gsl/1.13_1+darwin_i386/opt/local/lib/libgslcblas.dylib"
    "/bin/cyggslcblas-0.dll"
    "/usr/lib/libgslcblas.so.0"
    "/usr/lib/libgslcblas.so"
    "/usr/lib64/libgslcblas.so.0"
    "/usr/lib64/libgslcblas.so"
    "/usr/lib/i386-linux-gnu/libgslcblas.so"
    ))

 (define libgsl-paths
   (list
    "libgsl"
    "/opt/local/lib/libgsl.dylib"
    "/sw/lib/libgsl.dylib"
    "/usr/local/lib/libgsl.dylib"
    "/opt/local/var/macports/software/gsl/1.12_0/opt/local/lib/libgsl.dylib"
    "/opt/local/var/macports/software/gsl/1.13_1+darwin_i386/opt/local/lib/libgsl.dylib"
    "/bin/cyggsl-0.dll"
    "/usr/lib/libgsl.so.0"
    "/usr/lib/libgsl.so"
    "/usr/lib64/libgsl.so.0"
    "/usr/lib64/libgsl.so"
    "/usr/lib/i386-linux-gnu/libgsl.so"
    ))

 ;; goes through a list of library paths and opens
 ;; the first one it finds
 (define (find-lib open-proc path-list success?)
   (if (equal? path-list '())
       (begin
         (display "lib not found in church/external/gsl-paths.ss\n")
         #f)
       (let ((lib-pointer (open-proc (car path-list))))
         (if (success? lib-pointer)
             lib-pointer
             (find-lib open-proc (cdr path-list) success?)))))

 )
