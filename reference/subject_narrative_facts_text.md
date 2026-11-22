# Render subject facts as compact text

Turns the list from \[subject_narrative_facts()\] into the plain-text
block that is handed to a narrator. It is deterministic and data-only,
so it doubles as a readable, no-model summary of exactly what the
narrator was told.

## Usage

``` r
subject_narrative_facts_text(facts)
```

## Arguments

- facts:

  A list from \[subject_narrative_facts()\].

## Value

A single multi-line string (empty string for NULL facts).
