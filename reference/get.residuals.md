# Get pseudo residuals

The pseudo residuals were derived from the conditional probabilities of
the observations. At each time-step, the pseudo residual is the
probability of an observation occurring given the prior observations and
latter observations.

## Usage

``` r
get.residuals(model)
```

## Arguments

- model:

  fitted hydroState model object.

## Value

Data frame of residuals for each time-step

## Details

`get.residuals`

`get.residuals` retrieves residuals from the fitted model and exports
them as a data frame.

## Examples

``` r
# Load fitted model
data(model.annual.fitted.221201)

## Get residuals in a dataframe
get.residuals(model = model.annual.fitted.221201)
#> Error in getQhat(.Object@Qhat.object, .Object@input.data): no slot of name "constant" for this object of class "Qhat.boxcox"

```
