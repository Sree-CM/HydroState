# Fit hydroState model

`fit.hydroState` fits a single hydroState model (`build`) or multiple
models (`build.all`) using global optimization by differential evolution
[DEoptim](https://cran.r-project.org/package=DEoptim) library. If
fitting all models be sure to install and load the
[parallelly](https://cran.r-project.org/package=parallelly) library. The
fitting of all models may take hours or days, but the calibration can
occur in parallel if the
[parallelly](https://cran.r-project.org/package=parallelly) library is
installed and loaded.

## Usage

``` r
fit.hydroState(
  model,
  pop.size.perParameter = 10,
  max.generations = 500,
  doParallel = FALSE,
  ...
)
```

## Arguments

- model:

  built hydroState model object, hydroState.allModels object, or
  hydroState.subAnnual.allModels object

- pop.size.perParameter:

  integer that should be greater than or equal to the number of
  parameters in the model. The default is '10' and is sufficient for all
  models.

- max.generations:

  integer that will stop the optimizer when set number of generations
  are reached. The default is '500'.

- doParallel:

  TRUE/FALSE to perform fitting in parallel on all computer cores.
  Default is FALSE

- ...:

  additional options to change the optimization settings: reltol,
  print.iterations, etc. from the
  [DEoptim](https://cran.r-project.org/package=DEoptim) library

## Value

A fitted hydroState model

## Details

`fit.hydroState`

After a hydroState model object is built, the model is ready to fit to
the observed streamflow through minimizing the negative log-likelihood
function too calibrate model parameters. The only required input is the
given built hydroState model object or hydroState.allModels object (all
models from `build.all = TRUE`). When fitting all models, the models are
fitted from least to most complex (least to max amount of parameters).
Each model has a minimum of 5 and maximum of 20 calibration attempts to
outperform the prior reference model else the model is rejected. For
instance 'model.1State.normal.log.AR0' contains 3-parameters and is the
reference model for 'model.1State.normal.log.AR1' which contains
4-parameters. The objective function of 'model.1State.normal.log.AR1'
must calibrate the model with a lower negative log-likelihood than
'model.1State.normal.log.AR0'. These reference models are pre-defined,
but this function allows the user to edit the reference models in the
data.frame if needed using `summary`. Details on the likelihood function
is as follows:

The likelihood function is estimated as:

\\L\_{T} = \delta P(x\_{1}) + \Gamma \delta P(x\_{2})...\Gamma \delta
P(x\_{T})1'\\

where:

- \\\delta\\ is the initial state distribution, the initial probability
  of being in each state: \\\delta = \begin{pmatrix} \delta\_{1} \\ 1-
  \delta\_{1} \end{pmatrix}\\

- \\P(x)\\ is the \\m\\ x \\m\\ diagonal emissions matrix of the
  probability density for each state using a lower tail truncated
  Gaussian distribution or a two-parameter Gamma distribution:

  - \\f\_{Gau}(x=\hat{\_{obs}q\_{t}}; \mu = \hat{\_{t}q\_{i}}, \sigma =
    \sigma\_{i}, a = 0) =
    \frac{\phi(\frac{x-\mu}{\sigma})}{\sigma(1-\Phi(\frac{a-\mu}{\sigma}))}\\

  - \\f\_{Gam}(x = \hat{\_{obs}q\_{t}}; k =
    \frac{\hat{\_{t}q\_{i}}^2}{\sigma\_{i}^2}, \theta =
    \frac{\sigma\_{i}^2}{\hat{\_{t}q\_{i}}}) =
    \frac{x^{k-1}e^{\frac{x}{\theta}}}{\theta^{k}\Gamma(k)}\\

  - where \\\phi\\ is the probability density function for the standard
    normal distribution, \\\Phi\\ is the cumulative distribution
    function for the standard normal distribution, \\k\\ is the shape
    parameter, \\\theta\\ is the scale parameter, and \\\Gamma(k)\\ is
    the gamma function. For more details, refer to pg. 8–17 in
    Supplementary Materials of (Peterson TJ, Saft M, Peel MC & John A
    (2021), Watersheds may not recover from drought, Science, DOI:
    [doi:10.1126/science.abd5085](https://doi.org/10.1126/science.abd5085)
    ).

- \\\Gamma\\ is the transition matrix

- \\T\\ is the number of time-steps.

## Examples

``` r
# Load data
data(streamflow_annual_221201)

## Build default annual hydroState model
model = build(input.data = streamflow_annual_221201)

## Fit built model (runtime ~ 14 sec)
# \donttest{
model = fit.hydroState(model)
#> Error: unable to find an inherited method for function ‘getMean’ for signature ‘.Object = "QhatModel.homo.normal.linear", data = "data.frame"’
# }

## Fit all built models (runtime > several hours)
# Load data
data(streamflow_annual_221201)

## Build all annual models
all.annual.models = build.all(input.data = streamflow_annual_221201, siteID = '221201')

# Fit all (runtime > several hours)
# \donttest{
all.annual.models = fit.hydroState(all.annual.models)
#> Calibrating  36 Models.
#> ... Minimum number of calibration per model:  5
#> ... Maximum number of calibration per model:  20
#> ************************************
#> Calibrating Model: model.1State.truc.normal.boxcox.AR0.a0std
#> Reference model name for maximum acceptable calib. solution: 
#> Reference model maximum acceptable calib. solution: -Inf
#> 
#> 
#> ... Doing Calibration Trial  1  using DEstrategy: 2
#> Error: unable to find an inherited method for function ‘getMean’ for signature ‘.Object = "QhatModel.homo.normal.linear", data = "data.frame"’
# }
```
