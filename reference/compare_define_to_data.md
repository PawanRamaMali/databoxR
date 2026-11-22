# Compare a loaded dataset against its define metadata (smart hints)

For each variable present in the data and/or the define, reports whether
it appears in each, and lays the expected (define) label and type next
to the actual (data) label and type, flagging label mismatches.

## Usage

``` r
compare_define_to_data(data, mc, dataset)
```

## Arguments

- data:

  A loaded data.frame (e.g. from `read_sdtm_adam`).

- mc:

  A `Metacore` object.

- dataset:

  The dataset name in the define to compare against.

## Value

A data.frame, one row per variable, with comparison columns.
