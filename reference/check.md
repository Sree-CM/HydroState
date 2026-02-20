# Check reliability of state predictions

`check` After fitting the model, the reliability of the estimated states
can be assessed by generating synthetic state sequences and then
assessing how well the model identified them.

## Usage

``` r
check(model, n.samples = 1e+05)
```

## Arguments

- model:

  fitted `hydroState` model.

- n.samples:

  integer of samples to re-sample. Default is 100000.

## Value

A data frame is returned with a matrix depending on the number of
states. For a 2 state model, a 2x2 matrix is returned. The diagonal cell
estimates the probability of correctly identifying that state. The off
diagonals estimate the probability of incorrectly identifying a state
that in state 2.

## Details

`check`

This validates the model's states at each time-step through re-sampling
the input data and re-running the Viterbi algorithm. The input data is
duplicated 100 times, and a synthetic series is generated from the model
with sample states. This provides a time series of the transformed
streamflow observations that can be compared with observations of the
\`known' state. The Viterbi states of the re-sampled transformed
observations are inferred, and the probability of the inferred state
equaling the 'known' state is calculated.

## Examples

``` r
## Check reliability of state predictions (>5s to run)
# \donttest{
check(model = model.annual.fitted.221201)
#>                    Inferred State Low Inferred State Normal
#> Known State Low             0.5325531             0.4674469
#> Known State Normal          0.1787494             0.8212506
# }
```
