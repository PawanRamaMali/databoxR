# Variable-level metadata (data dictionary) from a define

Joins the define's variable, value-level and dataset-variable tables
into a single per-dataset data dictionary.

## Usage

``` r
define_var_metadata(mc, dataset = NULL)
```

## Arguments

- mc:

  A `Metacore` object.

- dataset:

  Optional dataset name to filter to.

## Value

A data.frame: dataset, variable, order, label, type, length, mandatory,
origin, codelist.
