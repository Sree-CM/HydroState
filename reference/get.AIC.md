# Get AIC

`get.AIC` retrieves Akaike information criteria from a fitted hydroState
model object or all models.

## Usage

``` r
get.AIC(model)
```

## Arguments

- model:

  fitted `hydroState` model object.

## Value

AIC value of a single model or a list variable of AIC values for al
models

## Details

`get.AIC`

The AIC is the negative log-likelihood of the model plus a penalty for
model parameters. This function can be performed on a single model or a
selection of models to find the lowest AIC of the set.

## Examples

``` r
# Load fitted model
data(model.annual.fitted.221201)

## AIC of a single model
get.AIC(model.annual.fitted.221201)
#> [1] 89.81635

## Lowest AIC of a model set
get.AIC(all.models.annual.fitted.407211)
#> ... The following model has a state with only 1  time points:  model.3State.normal.boxcox.AR2.a0std
#> ... The following model has a state with only 1  time points:  model.3State.normal.boxcox.AR3.a0std
#>                                                 AIC nParameters  AIC.weights
#> model.2State.gamma.boxcox.AR2.a0std        72.29543          10 4.539442e-01
#> model.2State.gamma.boxcox.AR3.a0std        72.73455          11 3.644593e-01
#> model.2State.gamma.boxcox.AR0.a0std        74.86469           8 1.256306e-01
#> model.2State.gamma.boxcox.AR1.a0std        76.77487           9 4.833987e-02
#> model.3State.gamma.boxcox.AR0.a0std        81.88396          15 3.757337e-03
#> model.3State.gamma.boxcox.AR1.a0std        83.87980          16 1.385125e-03
#> model.3State.gamma.boxcox.AR3.a0std        84.09137          18 1.246082e-03
#> model.3State.gamma.boxcox.AR2.a0std        84.84782          17 8.536605e-04
#> model.1State.gamma.boxcox.AR2.a0std        87.34379           5 2.450717e-04
#> model.1State.gamma.boxcox.AR3.a0std        89.29440           6 9.241094e-05
#> model.1State.gamma.boxcox.AR1.a0std        91.00718           4 3.924624e-05
#> model.2State.truc.normal.boxcox.AR0.a0std  96.02439           8 3.193921e-06
#> model.2State.truc.normal.boxcox.AR1.a0std  97.98971           9 1.195530e-06
#> model.2State.truc.normal.boxcox.AR2.a0std  98.54317          10 9.065239e-07
#> model.3State.truc.normal.boxcox.AR0.a0std  99.70401          15 5.073483e-07
#> model.2State.truc.normal.boxcox.AR3.a0std 100.34890          11 3.675108e-07
#> model.3State.truc.normal.boxcox.AR2.a0std 100.93170          17 2.746092e-07
#> model.3State.truc.normal.boxcox.AR1.a0std 101.44630          16 2.123109e-07
#> model.2State.normal.boxcox.AR0.a0std      102.08864           8 1.539893e-07
#> model.3State.truc.normal.boxcox.AR3.a0std 102.92497          18 1.013636e-07
#> model.1State.gamma.boxcox.AR0.a0std       103.31423           3 8.343658e-08
#> model.1State.truc.normal.boxcox.AR2.a0std 103.67500           5 6.966526e-08
#> model.2State.normal.boxcox.AR1.a0std      104.03954           9 5.805746e-08
#> model.1State.truc.normal.boxcox.AR3.a0std 104.98191           6 3.624304e-08
#> model.2State.normal.boxcox.AR3.a0std      105.64588          11 2.600426e-08
#> model.2State.normal.boxcox.AR2.a0std      105.84569          10 2.353182e-08
#> model.1State.truc.normal.boxcox.AR1.a0std 108.54189           4 6.112006e-09
#> model.1State.normal.boxcox.AR2.a0std      109.52988           5 3.729460e-09
#> model.1State.normal.boxcox.AR3.a0std      110.93876           6 1.843788e-09
#> model.1State.truc.normal.boxcox.AR0.a0std 112.14714           3 1.007662e-09
#> model.1State.normal.boxcox.AR1.a0std      112.76325           4 7.405059e-10
#> model.3State.normal.boxcox.AR0.a0std      113.33030          15 5.576917e-10
#> model.3State.normal.boxcox.AR1.a0std      115.42311          16 1.958600e-10
#> model.1State.normal.boxcox.AR0.a0std      115.55754           3 1.831282e-10
```
