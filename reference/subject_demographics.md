# Demographics summary for one subject

Reads the common DM variables (age, sex, race, arm, site, etc.) for the
subject and returns them as a tidy two-column table. Missing or empty
fields are omitted.

## Usage

``` r
subject_demographics(datasets, subject_id)
```

## Arguments

- datasets:

  A named list of data.frames.

- subject_id:

  A single subject ID.

## Value

A data.frame with columns Field and Value.
