# Get seasons

Aggregates monthly data to 4 seasons in a year.

## Usage

``` r
get.seasons(
  input.data = data.frame(year = c(), month = c(), flow = c(), precip = c())
)
```

## Arguments

- input.data:

  dataframe of monthly runoff and precipitation observations. Gaps with
  missing data in either streamflow or precipitation are permitted, and
  the handling of them is further discussed in `build`. Monthly data is
  required when using `seasonal.parameters` that assumes selected model
  parameters are better defined with a sinusoidal function.

## Value

A dataframe of seasonal observations with an additional column counting
the number of months in each season.

## Details

`get.seasons`

This function takes sums monthly runoff and precipitation observations
into 4 seasons of a year.

## Examples

``` r
# Load data
data(streamflow_monthly_221201)

# aggregate monthly data to seasonal
streamflow_seasonal_221201 = get.seasons(streamflow_monthly_221201)

```
