
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rfishprod

<!-- badges: start -->
<!-- badges: end -->

Implements a framework to generate individual-level estimates of fish
productivity, with a focus on reef fishes. This individual approach
works by combining estimates of somatic growth through the von
Bertalanffy Growth Model, and deterministic or stochastic natural
mortality using instantaneous mortality rates.

## Installation

You can install rfishprod from
[GitHub](https://github.com/renatoamorais/rfishprod) with:

``` r
devtools::install_github("renatoamorais/rfishprod")
```

To get
[`devtools`](https://cran.r-project.org/web/packages/devtools/index.html),
simply use `install.packages("devtools")`.

Although not a dependency, I recommend using R 3.6.3 or higher. This is
mainly because [`xgboost`](https://CRAN.R-project.org/package=xgboost),
the machine behind internal prediction, demands R 3.6.3 in its most
recent version.

## Updates

On 02.08.2022: included the empirical equation to estimate instant
mortality from Lorenzen et al (2022) Fish Res 252:106327. Also changed
the method = ‘Function’ to leave the exponent of the relationship
between mortality and body length adjustable by the user (it was fixed
as -0.75) given evidence of the same paper that this exp could be closer
to -1 (value of -0.91 set as default, see Table 2, model 3 in this ref).

## Example

``` r
library(rfishprod)

# Check dataset repdata #
(repdata <- rfishprod:::repdata)

# Getting levels ready #
repdata <- tidytrait (repdata, db)

# Formula from Morais and Bellwood (2018) #
fmod <- formula (~ sstmean + MaxSizeTL + Diet + Position + Method)

# Predicting Kmax, the standardised VBGF parameter (Recommendation: use 100s to 1000s iterations) #
datagr <- predKmax (repdata, 
                    dataset = db,
                    fmod = fmod,
                    niter = 10,
                    return = 'pred')

datagr <- datagr$pred

# Predicting M/Z: the instantaneous mortality rate (Recommendation: see help file for) #
datagr$Md <- with (datagr,
                   predM (Lmeas = Size,
                          Lmax = MaxSizeTL,
                          Kmax = Kmax,
                          method = 'Gislason'))
                           

# Positioning your fish in their growth trajectory #
# aka. what's the size they're supposed to have on the next day? #
with (datagr, applyVBGF (Lmeas = Size,
                         Lmax = MaxSizeTL,
                         Kmax = Kmax))
                         
# Compare with their size on the previous day #
datagr$Size

# Estimating gross somatic growth (g) #
with(datagr, somaGain (a = a,
                       b = b,
                       Lmeas = Size,
                       Lmax = MaxSizeTL,
                       Kmax = Kmax))
                              
# Applying stochastic mortality #
applyMstoch (datagr$Md)


# Alternatively, estimating per capita mass loss due to mortality #
with(datagr, somaLoss (M = Md,
                       Lmeas = Size,
                       a = a,
                       b = b))
```

## Citation

Please, if you’re using `rfishprod`, the relevant citation for the
package can be obtained from:

``` r
citation("rfishprod")
```

## Issues

Please [report issues or
bugs](https://github.com/renatoamorais/rfishprod/issues) or shoot me an
email (just hit `?rfishprod`)
