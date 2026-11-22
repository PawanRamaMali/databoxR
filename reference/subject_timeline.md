# Merged chronological timeline for one subject

Scans each domain's records for a start-date column (`--STDTC`, `--DTC`,
...) and an event-label column (`--TERM`, `--DECOD`, `--TRT`, `--TEST`),
then stacks the dated events across domains and orders them by date.
This is the cross-domain view: one subject's journey through the study
in one table. When a reference start date is available the CDISC study
day is added (day 1 is the reference date; there is no day 0).

## Usage

``` r
subject_timeline(datasets, subject_id, ref_date = NULL)
```

## Arguments

- datasets:

  A named list of data.frames.

- subject_id:

  A single subject ID.

- ref_date:

  Optional reference start Date; derived from DM RFSTDTC when NULL.

## Value

A data.frame with columns Date, Day, Domain, Event (zero rows if no
dated events are found).
