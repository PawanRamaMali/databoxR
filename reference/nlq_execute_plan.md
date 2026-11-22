# Execute a query plan deterministically

Validates the plan, then runs it: a \`filter\` action subsets the named
domain with vectorised comparisons, and a subject action calls the
matching \`subject\_\*\` helper. Never evaluates model-supplied text as
code.

## Usage

``` r
nlq_execute_plan(plan, datasets)
```

## Arguments

- plan:

  A query plan (named list).

- datasets:

  A named list of data frames.

## Value

A data frame of results (zero rows if nothing matches).
