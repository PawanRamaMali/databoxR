# Answer a natural-language question against loaded datasets

Orchestrates the grounded pipeline: build a schema, ask a planner (an
injectable function, so this is testable without a network) for a query
plan, validate it, and execute it deterministically. The planner only
proposes a structured plan; all data access happens here in R.

## Usage

``` r
nl_query(question, datasets, propose = nlq_propose_claude, ...)
```

## Arguments

- question:

  A natural-language question (character).

- datasets:

  A named list of data frames.

- propose:

  A function of \`(question, schema)\` returning a query plan. Defaults
  to \[nlq_propose_claude()\], which uses the ellmer package.

- ...:

  Passed to \`propose\`.

## Value

A list: \`plan\`, \`valid\` (validation result), \`result\` (data frame
or NULL), \`code\` (reproducible R) and \`error\` (character or NULL).
