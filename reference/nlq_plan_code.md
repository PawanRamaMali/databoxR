# Generate reproducible R code for a query plan

Emits the dplyr (filter action) or \`databoxR::subject\_\*\` (subject
action) call equivalent to the plan, so a result can be reproduced
outside the app.

## Usage

``` r
nlq_plan_code(plan)
```

## Arguments

- plan:

  A query plan (named list).

## Value

A single character string of R code.
