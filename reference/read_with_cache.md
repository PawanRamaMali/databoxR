# Enhanced file reader with caching

Wrapper around read_sdtm_adam with caching support

## Usage

``` r
read_with_cache(path, use_cache = TRUE, ...)
```

## Arguments

- path:

  File path

- use_cache:

  Logical, whether to use caching (default: TRUE)

- ...:

  Additional arguments passed to read_sdtm_adam

## Value

data.frame
