
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{latexer}`

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of `{latexer}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
latexer::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-03-23 10:36:54 EAT"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading latexer
#> 
#> Attaching package: 'jsonlite'
#> 
#> 
#> The following object is masked from 'package:shiny':
#> 
#>     validate
#> 
#> 
#> ✖ In topic 'run_app': @inheritParams failed.
#> ℹ All parameters are already documented; none remain to be inherited.
#> ── R CMD check results ───────────────────────────────── latexer 0.0.0.9000 ────
#> Duration: 3.4s
#> 
#> ❯ checking package dependencies ... ERROR
#>   Namespace dependency missing from DESCRIPTION Imports/Depends entries: ‘golem’
#>   
#>   See section ‘The DESCRIPTION file’ in the ‘Writing R Extensions’
#>   manual.
#> 
#> 1 error ✖ | 0 warnings ✔ | 0 notes ✔
#> Error: R CMD check found ERRORs
```

``` r
covr::package_coverage()
#> Error in loadNamespace(x): there is no package called 'covr'
```
