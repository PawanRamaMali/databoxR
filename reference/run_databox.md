# Run the databoxR Shiny Application

This function launches the main databoxR application for exploring
SDTM/ADaM datasets

## Usage

``` r
run_databox(demo_data = FALSE, ...)
```

## Arguments

- demo_data:

  Logical, if TRUE loads sample datasets automatically. Default is
  FALSE.

- ...:

  arguments to pass to shinyApp

## Value

A shiny application object

## Examples

``` r
if (FALSE) { # \dontrun{
  run_databox()
  run_databox(demo_data = TRUE)  # Load with sample data
} # }
```
