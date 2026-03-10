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
#> Error in `$<-.data.frame`(`*tmp*`, "Qhat.flow", value = numeric(0)): replacement has 0 rows, data has 94

## Lowest AIC of a model set
get.AIC(all.models.annual.fitted.407211)
#> Error in `$<-.data.frame`(`*tmp*`, "Qhat.flow", value = numeric(0)): replacement has 0 rows, data has 73
```
