# Validate a query plan against the loaded data

Checks that the plan's action, domain, columns and operators all refer
to things that actually exist, before any execution.

## Usage

``` r
nlq_validate_plan(plan, datasets)
```

## Arguments

- plan:

  A query plan (named list), as produced by a planner.

- datasets:

  A named list of data frames.

## Value

A list with \`ok\` (logical), \`errors\` and \`warnings\` (character).
