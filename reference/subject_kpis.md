# Headline counts for one subject

Summarises a subject across domains into a handful of KPIs suitable for
value boxes: the number of domains they appear in, total records,
adverse-event count, serious adverse events, worst AE severity, and
number of visits.

## Usage

``` r
subject_kpis(datasets, subject_id)
```

## Arguments

- datasets:

  A named list of data.frames.

- subject_id:

  A single subject ID.

## Value

A named list of KPI values.
