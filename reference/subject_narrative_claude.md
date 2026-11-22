# Narrate one subject's facts with Claude via ellmer

Default narrator for \[subject_narrative()\]. Sends the derived facts
text (not the raw data) to Claude and returns a short plain-language
summary. Requires the \`ellmer\` package and an \`ANTHROPIC_API_KEY\`.

## Usage

``` r
subject_narrative_claude(
  facts_text,
  facts = NULL,
  model = "claude-opus-5-5",
  chat = NULL,
  ...
)
```

## Arguments

- facts_text:

  The facts block from \[subject_narrative_facts_text()\].

- facts:

  The structured facts list (unused here; accepted so narrators share
  one signature).

- model:

  Claude model id (default \`"claude-opus-5-5"\`).

- chat:

  An optional pre-built ellmer chat object (mainly for testing).

- ...:

  Ignored.

## Value

A single character string: the narrative.
