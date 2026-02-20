# Builds hydroState model

`build` builds a hydrostate model object with either a default model or
the model can be specified with options from below. Every model depends
on a linear base model where streamflow, \\Q\\, is a function of
precipitation, \\P\\: \\\hat{Q} = Pa_1+a_0\\. The default model is a
variant of this base linear model with state shifts expected in the
intercept, \\a_0\\, and standard deviation, \\std\\, of the
rainfall-runoff relationship. There are additional options to adjust
this model with auto-correlation terms and seasonal parameters that can
be both independent of state or change with state. The number of states
and assumed error distribution can also be selected. After the model is
built, the hydroState model is ready to be fitted with
[`fit.hydroState`](https://peterson-tim-j.github.io/HydroState/reference/fit.hydroState.md)

## Usage

``` r
build(
  input.data = data.frame(year = c(), flow = c(), precip = c()),
  data.transform = "boxcox",
  parameters = c("a0", "a1", "std"),
  seasonal.parameters = NULL,
  state.shift.parameters = c("a0", "std"),
  error.distribution = "truc.normal",
  flickering = FALSE,
  transition.graph = matrix(TRUE, 2, 2)
)
```

## Arguments

- input.data:

  dataframe of annual, seasonal, or monthly runoff and precipitation
  observations. Gaps with missing data in either streamflow or
  precipitation are permitted. Monthly data is required when using
  `seasonal.parameters`.

- data.transform:

  character sting with the method of transformation. The default is
  'boxcox'. Other options: 'log', 'burbidge', 'none'

- parameters:

  character vector of parameters to construct model. Required and
  default: `a0`, `a1`, `std`. Auto-correlation terms optional: `AR1`,
  `AR2`, or `AR3`.

- seasonal.parameters:

  character vector of one or all parameters (`a0`, `a1`, `std`) defined
  as a sinusoidal function to represent seasonal variation. Requires
  monthly or seasonal data. Default is empty, no seasonal parameters.

- state.shift.parameters:

  character vector of one or all parameters (`a0`, `a1`, `std`, `AR1`,
  `AR2`, `AR3`) able to shift as dependent on state. Default is `a0` and
  `std`.

- error.distribution:

  character string of the distribution in the HMM error. Default is
  'truc.normal'. Others include: 'normal' or 'gamma'

- flickering:

  logical `TRUE`/`FALSE`. `TRUE` = allows more sensitive markov
  flickering between states over time. When `FALSE` (default), state
  needs to persist for at least three time steps before state shift can
  occur.

- transition.graph:

  matrix given the number of states. Default is a 2-state matrix (2 by
  2): `matrix(TRUE,2,2)`.

## Value

A built hydroState model object ready to be fitted with
[`fit.hydroState()`](https://peterson-tim-j.github.io/HydroState/reference/fit.hydroState.md)

## Details

`build`

There are a selection of items to consider when defining the
rainfall-runoff relationship and investigating state shifts in this
relationship. hydroState provides various options for modelling the
rainfall-runoff relationship.

- Data gaps with `input.data`: When there is missing `input.data` in
  either the dependent variable, streamflow, or independent variable,
  precipitation, the emissions probability of the missing time-step is
  set equal to one. This essentially ignores the missing periods. The
  time step after the missing period has a state probability dependent
  on the length of the gap. The larger the gap, the closer the state
  probability gets to approaching a finite probability near zero (as the
  transition probabilities are recursively multiplied). When the model
  has auto-correlation terms and there are gaps in the dependent
  variable, the auto-correlation function restarts at the beginning of
  each continuous period after the gap. This ignores auto-correlation at
  the first time steps after the gap. For instance, an 'AR1' model would
  ignore the contribution of the prior time step for the first (1)
  observation after the gap.

- Transform Observations with `data.transform`: Transforms streamflow
  observations to remove heteroscedasticity. Often there is skew within
  hydrologic data. When defining relationships between rainfall-runoff,
  this skew results in an unequal variance in the residuals,
  heteroscedasticity. Transforming streamflow observations is often
  required. There are several options to transform observations. Since
  the degree of transformation is not typically known, `boxcox` is the
  default. Other options include: `log`, `burbidge`, and of course,
  `none` when no transformation is performed.

- Model Structure with `parameters` and `seasonal.parameters`: The
  structure of the model depends on the `parameters`. hydroState
  simulates runoff, \\Q\\, as being in one of a finite states, \\i\\, at
  every time-step, \\t\\, depending on the distribution of states at
  prior time steps. This results in a runoff distribution for each state
  that can vary over time (\\\hat{\_tQ_i}\\). The model defines the
  relationship that is susceptible to state shifts with precipitation,
  \\P_t\\, as a predictor. This takes the form as a simple linear model
  \\\hat{\_tQ_i} = f(P_t)\\:

  \$\$ \hat{\_tQ_i} = P_ta_1 + a_0 \$\$

  where \\a_0\\ and \\a_1\\ are constant parameters. These parameters
  and the model error, \\std\\, establish the rainfall-runoff
  relationship and are required parameters for every built model object.
  These are the default parameters: `c('a0', 'a1', 'std')`.

- Auto-correlation `parameters`: The relationship may contain serial
  correlation and would be better defined with an auto-regressive term:

  \$\$ \hat{\_tQ_i} = P_ta_1 + a_0 + AR1\hat{\_{t-1}Q} \$\$

  where `AR1` is the lag-1 auto-correlation term. Either, lag-1: `AR1`,
  lag-2: `AR2`, and lag-3: `AR3` auto-correlation coefficients are an
  option as additional parameters to better define the rainfall-runoff
  relationship.

- Sub-annual analysis with `seasonal.parameters`: Additional options
  include explaining the seasonal rainfall-runoff relationship with a
  sinusoidal function that better defines either of the constant
  parameters or error (\\a_0, a_1, std\\) throughout the year, i.e:

  \$\$ a_0 = a\_{0.disp} + a\_{0.amp} \* sin(2\pi(\frac{M_t}{12} +
  a\_{0.phase})) \$\$

  where \\M_t\\ is an integer month at \\t\\. Monthly streamflow and
  precipitation are required as `input.data` for the sub-annual
  analysis.

- State Dependent Parameters with `state.shift.parameters`: These are
  state dependent parameters where they are subject to shift in order to
  better explain the state of streamflow over time. Any or all of the
  previously chosen parameters can be selected
  (`a_0, a_1, std, AR1, AR2, AR3`). The default model evaluates shifts
  in the rainfall-runoff relationship with `a_0` `std` as state
  dependent parameters.

- Distribution of the Residuals with `error.distribution`: The
  distribution of the residuals (error) within a state of the model can
  be chosen to reduce skew and assist with making models statistically
  adequate (see `plot(pse.residuals = TRUE)`). Either normal: `normal`,
  truncated normal: `truc.normal`, or gamma: `gamma` distributions are
  acceptable. These error distribution ensures streamflow is greater
  than zero `Q > 0`, and specifically for `truc.normal` greater than or
  equal to zero `Q >= 0`. The default is `truc.normal`. Sub-annual
  models are restricted to only a `gamma` distribution.

- Markov flickering with `flickering`: When flickering is `FALSE`, the
  markov avoids state shifts for very short duration, and hence for a
  state shift to occur it should last for an extended period. The
  default is FALSE. If TRUE, flickering between more states is more
  sensitive. For further explanation on this method, see: [Lambert et
  al., 2003](https://hess.copernicus.org/articles/7/652/2003/). The
  current form of the Markov model is homogeneous where the transition
  probabilities are time-invariant.

- Number of States with `transition.graph`: The number of possible
  states in the rainfall-runoff relationship and transition between the
  states is selected with the transition.graph. The default is a 2-state
  model in a 2 by 2 unstructured matrix with a TRUE transition to and
  from each state (i.e. `matrix(TRUE,2,2)`). hydroState accepts 1-state
  up to 3-states (i.e. for 3-state unstructured transition graph:
  `matrix(TRUE,3,3)`). The unstructured transition graph allows either
  state to remain in the current state or transition between any state.
  For the 3-state transition graph, one may want to assume the
  transitions can only occur in a particular order, as in (Very low -\>
  Low -\> Normal-\>) rather than (Very low \<-\> Low \<-\> Normal \<-\>
  Very low). Thus, a structured graph is also acceptable (3-state
  structured:
  `matrix(c(TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,TRUE),3,3)`).
  More details in Supplementary Materials of (Peterson TJ, Saft M, Peel
  MC & John A (2021), Watersheds may not recover from drought, Science,
  DOI:
  [doi:10.1126/science.abd5085](https://doi.org/10.1126/science.abd5085)
  ).

## Examples

``` r
# Load data
data(streamflow_annual_221201)

## Build default annual hydroState model
model = build(input.data = streamflow_annual_221201)

# OR

## Build annual hydroState model with specified objects
# Build hydroState model with: 2-state, normal error distribution,
# 1-lag of auto-correlation, and state dependent parameters ('a1', 'std')
model = build(input.data = streamflow_annual_221201,
                   data.transform = 'boxcox',
                   parameters = c('a0','a1','std','AR1'),
                   state.shift.parameters = c('a1','std'),
                   error.distribution = 'normal',
                   flickering = FALSE,
                   transition.graph = matrix(TRUE,2,2))
```
