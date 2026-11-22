# Describe loaded datasets for a query planner

Builds a compact, data-free schema (domains, their columns, column types
and a few distinct values for small categorical columns) suitable for
handing to a language model so it can propose a query plan. No row-level
data leaves.

## Usage

``` r
nlq_schema(datasets, max_levels = 8)
```

## Arguments

- datasets:

  A named list of data frames (loaded domains).

- max_levels:

  Maximum distinct values to list for a categorical column.

## Value

A list with \`subjects\` (character) and \`domains\`: a named list, one
entry per domain, each a data frame of \`column\`, \`type\`, \`values\`.
