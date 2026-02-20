# Summarize all models

`summary` outputs a summary table of all built hydroState models and
allows users to edit the reference models for calibration.

## Usage

``` r
# S3 method for class 'hydroState.allModels'
summary(object, ...)
```

## Arguments

- object:

  hydroState.allModels object with a list of models from `build.all`

- ...:

  No additional input required

## Value

A data.frame with a summary table of all models and reference models

## Details

`summary`

For every model object in `build.all`, there is a reference model for
calibration. The reference model is a slightly simpler model with one
less parameter. During calibration with `fit`, the model performance
must exceed the the performance of the reference model else the model is
rejected. This function is used to output the `summary.table` and adjust
the reference models. Afterwards, all models can be re-build with
including this `summary.table` in the `build.all` function.

## Examples

``` r
# Show summary table of all fitted model details and reference models

all.models.ref.table = summary(all.models.annual.fitted.407211)
```
