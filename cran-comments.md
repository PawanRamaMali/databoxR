# CRAN comments

## Submission

This is a new submission. databoxR provides a Shiny interface for exploring
SDTM and ADaM clinical trial datasets.

## Test environments

* Local: Windows 11, R 4.6.2, `R CMD check --as-cran`
* GitHub Actions (r-lib/actions): macOS, Windows, and Ubuntu with R release,
  R devel, and R oldrel-1

## R CMD check results

0 errors | 0 warnings | 2 notes

* Note: "New submission". This is the first submission of the package.
* Note: README.md / NEWS.md "cannot be checked without 'pandoc' being
  installed". This appears only on the local machine, which has no pandoc;
  pandoc is available on the CRAN check machines.

The example for `run_databox()` is wrapped in `\dontrun{}` because it starts an
interactive Shiny application.

## Downstream dependencies

There are currently no downstream dependencies.
