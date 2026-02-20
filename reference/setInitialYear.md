# Sets state names given initial year

sets the state names for each time-step relative to the initial year
given

## Usage

``` r
setInitialYear(model, initial.year)
```

## Arguments

- model:

  fitted hydroState model object.

- initial.year:

  integer with year (YYYY). Default is first year in input.data.

## Value

A fitted hydroState model object with state names for each time-step
ready for `plot`

## Details

`setInitialYear`

hydroState assigns names to the computed states. This requires choosing
an initial year where the state value from that year will be named
'Normal'. Other state values will be given names relative to the state
value in the initial year. The choice of the initial year does not
affect results. It is a means to more easily interpret the difference in
state values relative to each other. It is best to choose a year based
on the question being asked. For example, in testing the impact of
drought, a year before the beginning of the drought, 1990, was selected
as an initial year when conditions were considered 'Normal' (Peterson
TJ, Saft M, Peel MC & John A (2021), Watersheds may not recover from
drought, Science, DOI:
[doi:10.1126/science.abd5085](https://doi.org/10.1126/science.abd5085) )

## Examples

``` r
# Load fitted model
data(model.annual.fitted.221201)

## Set initial year to set state names
model.annual.fitted.221201 =
                setInitialYear(model = model.annual.fitted.221201,
                               initial.year = 1990)

```
