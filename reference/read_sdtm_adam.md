# Read SDTM/ADaM Datasets with Enhanced Features

Reads SDTM/ADaM datasets from various formats (XPT, CSV, TXT) with
optional row limiting, column selection, and performance optimizations.

## Usage

``` r
read_sdtm_adam(
  path,
  nrows = NULL,
  cols = NULL,
  encoding = "UTF-8",
  validate_data = TRUE,
  progress_callback = NULL
)
```

## Arguments

- path:

  File path to the dataset

- nrows:

  Maximum number of rows to read (NULL for all rows)

- cols:

  Vector of column names or indices to select (NULL for all columns)

- encoding:

  Character encoding (default: "UTF-8")

- validate_data:

  Logical, whether to perform data validation (default: TRUE)

- progress_callback:

  Function to call with progress updates (optional)

## Value

A data.frame containing the dataset

## Examples

``` r
if (FALSE) { # \dontrun{
  # Read all data
  data <- read_sdtm_adam("path/to/dataset.xpt")
  
  # Read first 1000 rows only
  data <- read_sdtm_adam("path/to/large_dataset.csv", nrows = 1000)
  
  # Read specific columns
  data <- read_sdtm_adam("path/to/dataset.xpt", cols = c("USUBJID", "AGE"))
} # }
```
