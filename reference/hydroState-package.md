# hydroState: Hidden Markov modeling for hydrological state change

`hydroState` is an R-package that identifies regime changes in
streamflow time-series that is not explained by variations in
precipitation.

## Details

For details of the mathematics, and its use in understanding catchment
drought non-recovery, see:

Peterson TJ, Saft M, Peel MC & John A (2021), Watersheds may not recover
from drought, Science, DOI: 10.1126/science.abd5085

The package allows a flexible set of Hidden Markov Models (HMMs) of
annual, seasonal or monthly time-step to be built and which includes
precipitation as a predictor of streamflow. Suites of models can be
build for a single catchment, ranging from from one to three states and
each with differing combinations of error models and auto-correlation
terms, allowing the most parsimonious model to easily be identified (by
AIC). The entire package is written in R S4 object oriented code with
user facing functions and vignettes.

## Functions

- [`build`](https://peterson-tim-j.github.io/HydroState/reference/build.md)
  — Builds hydroState model

- [`build.all`](https://peterson-tim-j.github.io/HydroState/reference/build.all.md)
  — Builds all hydroState models

- [`fit.hydroState`](https://peterson-tim-j.github.io/HydroState/reference/fit.hydroState.md)
  — Fit hydroState model(s)

- [`setInitialYear`](https://peterson-tim-j.github.io/HydroState/reference/setInitialYear.md)
  — Sets state names given initial year

- [`plot.hydroState`](https://peterson-tim-j.github.io/HydroState/reference/plot.hydroState.md)
  — Plot states or pseudo residuals over time

- [`get.residuals`](https://peterson-tim-j.github.io/HydroState/reference/get.residuals.md)
  — Get pseudo residuals

- [`get.states`](https://peterson-tim-j.github.io/HydroState/reference/get.states.md)
  — Get states

- [`check`](https://peterson-tim-j.github.io/HydroState/reference/check.md)
  — Check reliability of state predictions

- [`get.AIC`](https://peterson-tim-j.github.io/HydroState/reference/get.AIC.md)
  — Get AIC

- [`get.seasons`](https://peterson-tim-j.github.io/HydroState/reference/get.seasons.md)
  — Get data as seasons

## Vignettes

- [Getting
  Started](https://peterson-tim-j.github.io/HydroState/articles/hydroState.html)

- [Adjust the default state
  model](https://peterson-tim-j.github.io/HydroState/articles/adjust.state.model.html)

- [Seasonal and monthly
  models](https://peterson-tim-j.github.io/HydroState/articles/subAnnual.models.html)

## Usage

The package contains a default `hydroState` model object that explains
streamflow as a function of precipitation using a linear model. Once the
model object is built
([`build`](https://peterson-tim-j.github.io/HydroState/reference/build.md)),
the model is fitted
([`fit.hydroState`](https://peterson-tim-j.github.io/HydroState/reference/fit.hydroState.md))
to determine the most likely rainfall-runoff state at each time-step. To
assess the adequacy of the fit, the residuals are plotted
([`plot`](https://rdrr.io/r/graphics/plot.default.html), and an adequate
fit requires the residuals to be normally distributed, uniform, with
minimal correlation and minimal trends. The resulting runoff states from
the fitted model can then be evaluated over time
([`plot`](https://rdrr.io/r/graphics/plot.default.html)) and even
exported
([`get.states`](https://peterson-tim-j.github.io/HydroState/reference/get.states.md))
with the state values, confidence intervals, and conditional
probabilities at each time-step. Input data requires a dataframe with
catchment average runoff and precipitation at annual, seasonal, or
monthly timesteps, and gaps with missing data are permitted. An example
of this workflow with the default model is demonstrated within the
[Getting
Started](https://peterson-tim-j.github.io/HydroState/articles/hydroState.html)
article.

To better explain the rainfall-runoff relationship, the default model
can be adjusted by selecting various items within the
[`build`](https://peterson-tim-j.github.io/HydroState/reference/build.md)
function. These include:

- `data.transform`: transform streamflow observations in order to reduce
  skew: 'boxcox', 'log', 'burbidge', or 'none'

- `parameters`: account for auto-correlation through including the
  degree of auto-correlation: 'AR1', 'AR2', or 'AR3'

- `state.shift.parameters`: assign either the intercept, slope, or
  auto-correlation parameter as state dependent parameter

- `error.distribution`: adjust the error distribution with 'normal',
  'gamma', or 'truc.normal'

- `seasonal.parameters`: account for intra-annual varation within the
  rainfall-runoff relationship

- `transition.graph`: set the number of possible states in the model (1,
  2, or 3).

An example of how to adjust the default model is demonstrated within the
[Adjust the default state
model](https://peterson-tim-j.github.io/HydroState/articles/adjust.state.model.html)
article and [Seasonal and monthly
models](https://peterson-tim-j.github.io/HydroState/articles/subAnnual.models.html)
article.

There is an additional option to construct all possible types of models
using the
[`build.all`](https://peterson-tim-j.github.io/HydroState/reference/build.all.md),
and compare them using the same
[`fit.hydroState`](https://peterson-tim-j.github.io/HydroState/reference/fit.hydroState.md)
function. The most likely model can be selected based on the AIC where
the best model will have the lowest AIC. An example of this is
demonstrated at the end of the [Adjust the default state
model](https://peterson-tim-j.github.io/HydroState/articles/adjust.state.model.html)
article and [Seasonal and monthly
models](https://peterson-tim-j.github.io/HydroState/articles/subAnnual.models.html)
article. To get stated, it is recommended to evaluate the default model
at first with one state and again with two states.

## Acknowledgments

The package development was funded by the Victorian Government The
Department of Energy, Environment, and Climate Action
(https://www.water.vic.gov.au/).

## See also

Useful links:

- <https://github.com/peterson-tim-j/HydroState>

- <https://peterson-tim-j.github.io/HydroState/>

- Report bugs at <https://github.com/peterson-tim-j/HydroState/issues>

## Author

**Maintainer**: Tim Peterson <tim.peterson@monash.edu>
([ORCID](https://orcid.org/0000-0002-1885-0826)) \[copyright holder\]

Authors:

- Thomas Westfall <thomas.westfall1@monash.edu>
  ([ORCID](https://orcid.org/0009-0000-0529-881X))
