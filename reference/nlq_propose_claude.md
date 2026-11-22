# Propose a query plan with Claude via ellmer

Default planner for \[nl_query()\]. Sends the question and the data-free
schema to Claude and returns a structured query plan. Requires the
\`ellmer\` package and an \`ANTHROPIC_API_KEY\`.

## Usage

``` r
nlq_propose_claude(question, schema, model = "claude-opus-5-5", chat = NULL)
```

## Arguments

- question:

  A natural-language question (character).

- schema:

  A schema from \[nlq_schema()\].

- model:

  Claude model id (default \`"claude-opus-5-5"\`).

- chat:

  An optional pre-built ellmer chat object (mainly for testing).

## Value

A query plan (named list) for \[nlq_validate_plan()\] /
\[nlq_execute_plan()\].
