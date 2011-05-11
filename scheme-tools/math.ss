#!r6rs

(library

 (scheme-tools math)

 (export pi
         logistic
         lnfact
         lngamma
         random-integer
         random-real
         binomial-pdf
         gamma-lnpdf
         dirichlet-lnpdf
         poisson-pdf
         gaussian-lnpdf
         tdist-pdf
         discrete-pdf
         mean
         mmultinomial-lnpdf
         normalize
         sample-discrete
         sample-binomial
         sample-poisson
         sample-gaussian
         sample-generalized-tdist
         sample-tdist
         sample-gamma
         sample-dirichlet
         sample-mmultinomial
         seed-rng
         randomize-rng
         sum)

 (import (scheme-tools math math))

 )