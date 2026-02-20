# Builds all hydroState models

`build.all` builds all possible combinations of hydroState models. The
same fields are available as in
[`build`](https://peterson-tim-j.github.io/HydroState/reference/build.md)
in order to specify the type of models to be built. After all models are
built, they are fitted using the same
[`fit.hydroState()`](https://peterson-tim-j.github.io/HydroState/reference/fit.hydroState.md)
function.

## Usage

``` r
build.all(
  input.data = data.frame(year = c(), flow = c(), precip = c()),
  data.transform = NULL,
  parameters = NULL,
  seasonal.parameters = NULL,
  state.shift.parameters = NULL,
  error.distribution = NULL,
  flickering = FALSE,
  transition.graph = NULL,
  summary.table = NULL,
  siteID = NULL
)
```

## Arguments

- input.data:

  dataframe of annual, seasonal, or monthly runoff and precipitation
  observations. Gaps with missing data in either streamflow or
  precipitation are permitted, and the handling of them is further
  discussed in `build`. Monthly data is required when using
  `seasonal.parameters` that assumes selected model parameters are
  better defined with a sinusoidal function.

- data.transform:

  character string of method of transformation. If empty, the default
  builds all possible combinations of models with `boxcox` data
  transformation.

- parameters:

  character vector of parameters to determine model form. If empty, the
  default builds all possible combinations of model forms.

- seasonal.parameters:

  character vector of parameters with sinusoidal function to represent
  seasonal variation. Requires monthly or seasonal data. If empty and
  monthly or seasonal data is given, the default builds all possible
  combinations of models with a seasonal parameter for each and all
  parameters.

- state.shift.parameters:

  character vector of one or all parameters to identify state dependent
  parameters. Only one set of parameters permitted. If empty, the
  default builds all possible model combinations with `c('a0','std')` as
  state shift parameters.

- error.distribution:

  character string of the distribution in the HMM error. If empty, the
  default builds models with all possible combinations of error
  distribution: `c('truc.normal', 'normal','gamma')`

- flickering:

  logical `TRUE`/`FALSE`. `TRUE` = allows more sensitive markov
  flickering between states over time. When `FALSE` (default), state
  needs to persist for at least three time steps before state shift can
  occur.

- transition.graph:

  matrix given the number of states. If empty, the default builds models
  with all possible combinations of states: 1-state matrix (1 by 1):
  `matrix(TRUE,1,1)`, 2-state matrix (2 by 2): `matrix(TRUE,2,2)`,
  3-state matrix (3 by 3): `matrix(TRUE,3,3)`.

- summary.table:

  data frame with a table summarizing all built models and corresponding
  reference model. From function
  [`summary()`](https://rdrr.io/r/base/summary.html). If empty, summary
  table will be built automatically.

- siteID:

  character string of site identifier.

## Value

A list of built hydroState models with every combination of objects
ready to be fitted

## Details

`build.all`

All possible combinations of hydroState models are built for each
auto-correlation lag and residual distribution from 1 to 3 states for a
specified data transformation. This allows for investigation of state
changes in the `state.shift.parameters`: the intercept `c('a0', 'std')`
or slope `c('a1', 'std')`. To reduce the number of models in the search,
specify which field(s) to remain constant. For example, to investigate
the best model with the number of auto-correlation terms and number of
states with a `boxcox` data transform and `gamma` distribution of the
residuals, set `data.transform` to `boxcox` and `error.distribution` to
`gamma`. If no fields are specified, all possible model combinations are
built. If investigating state shifts in the intercept `a0` and slope
`a1`, it is recommended to build and fit the model combinations
separately.

## Examples

``` r
# Load data
data(streamflow_annual_221201)

# Build all annual models with state shift in intercept 'a0'
all.annual.models = build.all(input.data = streamflow_annual_221201,
                                    state.shift.parameters = c('a0','std'),
                                     siteID = '221201')

# OR

# Build all annual models with state shift in slope 'a1'
all.annual.models = build.all(input.data = streamflow_annual_221201,
                                    state.shift.parameters = c('a1','std'),
                                     siteID = '221201')
```
