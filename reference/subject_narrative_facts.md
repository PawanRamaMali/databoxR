# Grounded facts for one subject

Reduces a subject's records across every loaded domain to a compact,
fully derived set of facts: demographics, headline KPIs, per-domain
record counts, the adverse-event list, and the span of the dated
timeline. Nothing here is inferred; every value comes straight from the
data. This is the only input a narrator is given, which is what keeps a
generated summary grounded.

## Usage

``` r
subject_narrative_facts(datasets, subject_id)
```

## Arguments

- datasets:

  A named list of data.frames.

- subject_id:

  A single subject ID.

## Value

A named list of facts, or NULL if the subject has no records.
