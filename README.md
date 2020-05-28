
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
simply use `install.packages("devtools")`

## Example

``` r
library(rfishprod)

# Check dataset repdata
repdata

# Getting levels ready #
repdata <- tidytrait (repdata, db)

# Formula from Morais and Bellwood (2018) #
fmod <- formula (~ sstmean + MaxSizeTL + Diet + Position + Method)

# Predicting Kmax per se (in reality, use 100s to 1000 iterations) #
datagr <- predKmax (repdata, 
                    dataset = db, 
                    fmod = fmod, 
                    params = xgboostparams, 
                    niter = 10, 
                    return = 'pred')

datagr <- datagr$pred

# Predicting M #
datagr$Md <- with (datagr, 
                    predM (Lmeas = Size, 
                             Lmax = MaxSizeTL, 
                             Kmax = Kmax, 
                             Lr = 1, 
                             temp = sstmean, 
                             method = 'Function'))
                           

# Applying the growth trajectory, aka what's the size of the fish in the next day?#
with (datagr, applyVBGF (Lmeas = Size, 
                         Lmax = MaxSizeTL, 
                         Kmax = Kmax))
                         
# Compare with the size the previosu day
datagr$Size

# Estimating gross somatic growth (g) #
sogr <- with(datagr, somaGain (a = a,
                               b = b,
                               Lmeas = Size, 
                               Lmax = MaxSizeTL, 
                               Kmax = Kmax))
                              
# Applying stochastic mortality #
applyMstoch (datagr$Md)


# Alternatively, estimating per capita loss due to mortality #
loss <- with(datagr, somaLoss (M = Md,
                               Lmeas = Size,
                               a = a,
                               b = b))
```

## Citation

Please, if you’re using `rfishprod`, the relevant citation for the
package can be obtained from:

``` r
citation("rfishprod")
#> 
#> To cite rfishprod in publications use:
#> 
#>   Morais, RA; Bellwood, DR. Principles for estimating fish productivity on coral reefs. Coral Reefs. In review.
#> 
#> If using predKmax, please also consider citing:
#> 
#>   Morais, RA; Bellwood, DR (2018) Global drivers of reef fish growth. Fish and Fisheries, 19(5): 874-889. DOI:
#>   10.1111/faf.12297
#> 
#> To see these entries in BibTeX format, use 'print(<citation>, bibtex=TRUE)', 'toBibtex(.)', or set
#> 'options(citation.bibtex.max=999)'.
```

## Issues

Please [report issues or
bugs](https://github.com/renatoamorais/rfishprod/issues) or shoot me an
email (just hit `?rfishprod`)
