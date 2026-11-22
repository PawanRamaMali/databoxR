# Load a define.xml into a metacore object

Wraps
[`metacore::define_to_metacore()`](https://atorus-research.github.io/metacore/reference/define_to_metacore.html).
The define.xml is parsed into a tidy metadata object (datasets,
variables, value-level metadata, codelists, derivations) that the other
functions here read from.

## Usage

``` r
load_define(path, verbose = "silent")
```

## Arguments

- path:

  Path to a define.xml file.

- verbose:

  Passed to metacore; one of "silent", "message", "warn", "collapse"
  (default "silent").

## Value

A `Metacore` object.
