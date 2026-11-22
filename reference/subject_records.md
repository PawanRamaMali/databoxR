# Pull one subject's records from every domain

Pull one subject's records from every domain

## Usage

``` r
subject_records(datasets, subject_id)
```

## Arguments

- datasets:

  A named list of data.frames.

- subject_id:

  A single subject ID.

## Value

A named list holding, for each domain that contains the subject, the
subset of rows for that subject (domains with no matching rows are
dropped).
