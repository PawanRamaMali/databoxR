# Generate a grounded narrative for one subject

Builds the subject's facts, hands them (and only them) to a narrator,
and returns the narrator's short plain-language summary together with
the facts it was given. The default narrator calls Claude via
\[subject_narrative_claude()\]; pass your own \`narrate\` (a function of
the facts text) to run without a model, for example in tests. The model
is never shown raw data or asked for anything but prose, so the result
stays grounded in the computed facts.

## Usage

``` r
subject_narrative(datasets, subject_id, narrate = NULL, ...)
```

## Arguments

- datasets:

  A named list of data.frames.

- subject_id:

  A single subject ID.

- narrate:

  A function taking the facts text (character) and returning a narrative
  string. Defaults to \[subject_narrative_claude()\].

- ...:

  Passed to \`narrate\`.

## Value

A list with elements \`facts\`, \`facts_text\`, \`narrative\`, and
\`error\` (one of narrative/error is non-NULL on success/failure).
