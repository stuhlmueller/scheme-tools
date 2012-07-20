#!r6rs

(library

 (scheme-tools math math)

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
         multinomial
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
         sum
         logsumexp
         log1minus
         uniform-draw
         flip
         log-flip
         LOG-PROB-1
         LOG-PROB-0)

 (import (rnrs)
         (only (srfi :1) first)
         (scheme-tools external)
         (scheme-tools math gsl-bindings))


 (define pi 3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093844609550582231725359408128481117450284102701938521105559644622948954930381964428810975665933446128475648233786783165271201909145648566923460348610454326648213393607260249141273724587006606315588174881520920962829254091715364367892590360011330530548820466521384146951941511609433057270365759591953092186117381932611793105118548074462379962749567351885752724891227938183011949129833673362440656643086021394946395224737190702179860943702770539217176293176752384674818467669405132000568127145263560827785771342757789609173637178721468440901224953430146549585371050792279689258923542019956112129021960864034418159813629774771309960518707211349999998372978049951059731732816096318595024459455346908302642522308253344685035261931188171010003137838752886587533208381420617177669147303598253490428755468731159562863882353787593751957781857780532171226806613001927876611195909216420198938095257201065485863278865936153381827968230301952035301852968995773622599413891249721775283479131515574857242454150695950829533116861727855889075098381754637464939319255060400927701671139009848824012858361603563707660104710181942955596198946767837449448255379774726847104047534646208046684259069491293313677028989152104752162056966024058038150193511253382430035587640247496473263914199272604269922796782354781636009341721641219924586315030)

 (define (logistic x) (/ 1 (+ 1 (exp (- x)))))


 ;; GSL

 (define lnfact gsl-lnfact)
 (define lngamma gsl-lngamma) ;;now defined in AD..

 ;;Note: scorers for continuous erps and erps with continuous parameters should be defined as scheme code or in AD with their derivatives.
 ;;FIXME: make dirichlet-lnpdf, poisson-pdf, discrete-pdf, etc AD friendly.
 (define binomial-pdf gsl-binomial-pdf)
 (define (gamma-lnpdf value a b) (- (* (- a 1) (log value)) (/ value b) (lngamma a) (* a (log b))))
 (define (lnbeta alphas) (- (apply + (map lngamma alphas)) (lngamma (apply + alphas))))
 (define dirichlet-lnpdf gsl-dirichlet-lnpdf) ;;FIXME: swap gsl for the below to ennable AD....
 ;;(define (dirichlet-lnpdf vals alphas) (- (apply + (map (lambda (v a) (* (- a 1) (log v))) vals alphas)) (lnbeta alphas)))
 (define poisson-pdf gsl-poisson-pdf)
 (define discrete-pdf gsl-discrete-pdf)
 ;; the constant 1.837... is log(2 pi)
 (define (gaussian-lnpdf val mu var)
   (* -0.5 (+ (+ 1.8378770664093453 (log var)) (/ (* (- val mu) (- val mu)) var) )) )
 (define tdist-pdf gsl-tdist-pdf)
 (define mmultinomial-lnpdf gsl-multinomial-lnpdf)

 (define random-integer gsl-rng-uniform-int)
 (define random-real gsl-rng-uniform-pos)
 (define sample-binomial gsl-binomial)
 (define (sample-gamma x y) (gsl-gamma x y))
 (define (sample-dirichlet a) (gsl-dirichlet a))
 (define (sample-poisson x) (gsl-poisson x))
 (define (sample-discrete x) (gsl-discrete x))
 (define (sample-gaussian mu sigma) (+ mu (gsl-gaussian sigma)))
 (define sample-tdist gsl-tdist)
 (define (sample-generalized-tdist nu mu sigma-sq) (+ (* (gsl-tdist nu) (sqrt sigma-sq)) mu))
 (define sample-mmultinomial gsl-multinomial)

 (define seed-rng gsl-rng-set)
 (define randomize-rng gsl-rng-randomize)

 (define (sum lst)
   (apply + lst))

 (define (mean lst)
   (/ (sum lst) (length lst)))

 (define (normalize lst)
   (let ([total (sum lst)])
     (map (lambda (element) (/ element total))
          lst)))

 ;; (define (logsumexp . log-vals)
 ;;   (let ([max-log-val (apply max log-vals)])
 ;;     (if (equal? max-log-val -inf.0)
 ;;         -inf.0
 ;;         (+ (log (exact->inexact (sum (map (lambda (val) (exp (- val max-log-val))) log-vals))))
 ;;            max-log-val))))

 (define (logsumexp . log-vals)
   (log (sum (map exp log-vals))))

 (define (log1minus v)
   (log (- 1.0 (exp v))))

 (define (uniform-draw lst)
   (list-ref lst (random-integer (length lst))))

 (define (flip . w)
   (if (null? w)
       (< (random-real) 0.5)
       (< (random-real) (car w)) ))

 (define (log-flip . w)
   (if (null? w)
       (flip .5)
       (flip (exp (car w)))))

 (define (multinomial vals ps)
   (list-ref vals (sample-discrete ps)))

 (define LOG-PROB-1 0)

 (define LOG-PROB-0 -inf.0)

 )