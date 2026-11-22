# Safe Column Selection

Safely select columns from a dataset with validation

## Usage

``` r
safe_select_columns(
  data,
  cols,
  default_cols = names(data)[1:min(5, ncol(data))]
)
```

## Arguments

- data:

  A data.frame

- cols:

  Column names or indices

- default_cols:

  Default columns to return if selection fails

## Value

Selected columns as data.frame
