# Get states

`get.states` uses the Viterbi algorithm to globally decode the model and
estimate the most probable sequence of states.

## Usage

``` r
get.states(model)
```

## Arguments

- model:

  fitted `hydroState` model object.

## Value

data frame of results to evaluate the rainfall-runoff states over time

## Details

`get.states`

These dataframe of results include:

- time-step: year and possibly either season or month for subannual
  analysis

- Viterbi State Number: state number (i.e. 1, 2, or 3) to differentiate
  states

- Obs. flow: streamflow observations

- Viterbi Flow: flow values of the Viterbi state including the 5\\

- Normal State Flow: flow values of the normal state including the 5\\

- Conditional Prob: conditional probabilities for each state show the
  probability of remaining in the given state. When the conditional
  probability is closer to 1, there is a higher probability that
  hydroState model remains in that state for the next time-step.

- Emission Density: emission density for each state is the result of
  multiplying the conditional probabilities by the transition
  probabilities at each timestep.

## Examples

``` r
# Load fitted model
data(model.annual.fitted.221201)

## Set initial year to set state names
model.annual.fitted.221201 =
                setInitialYear(model = model.annual.fitted.221201,
                               initial.year = 1990)

## Get states
model.annual.fitted.221201.states =
                get.states(model = model.annual.fitted.221201)
```
