# Reproducible R code that extracts one subject across domains

Reproducible R code that extracts one subject across domains

## Usage

``` r
subject_profile_code(subject_id, domains, libraries = "dplyr")
```

## Arguments

- subject_id:

  A single subject ID.

- domains:

  Character vector of domain object names to filter.

- libraries:

  Packages to emit [`library()`](https://rdrr.io/r/base/library.html)
  calls for.

## Value

A single multi-line string of runnable R.
