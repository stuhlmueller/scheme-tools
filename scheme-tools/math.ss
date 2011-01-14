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
         mmultinomial-lnpdf
         sample-discrete
         sample-binomial
         sample-poisson
         sample-gaussian
         sample-generalized-tdist
         sample-tdist
         sample-gamma
         sample-dirichlet
         sample-mmultinomial
         seed-rng randomize-rng)

 (import (scheme-tools math math))

 )