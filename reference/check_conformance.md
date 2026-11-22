# Metadata-driven conformance checks against a define

Produces a tidy, severity-ranked set of findings comparing a loaded
dataset to its define metadata:

- missing variables (Error if the define marks them mandatory, otherwise
  Warning) and unexpected variables not in the define (Info);

- variable label mismatches (Warning); and

- values that fall outside the define's controlled terminology (Error),
  via
  [`metatools::get_bad_ct()`](https://pharmaverse.github.io/metatools/reference/get_bad_ct.html).

## Usage

``` r
check_conformance(data, mc, dataset)
```

## Arguments

- data:

  A loaded data.frame.

- mc:

  A `Metacore` object.

- dataset:

  The dataset name in the define to check against.

## Value

A data.frame with columns Check, Variable, Severity, Details.
