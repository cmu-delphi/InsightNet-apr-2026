
<!-- README.md is generated from README.Rmd. Please edit that file -->

# InsightNetApr26

<!-- badges: start -->
<!-- badges: end -->

The goal of `{InsightNetApr26}` is to allow attendees of the workshop
to install all necessary packages at once.

This repository is based on [micom-tooling-workshop-2025-dependencies](https://github.com/cmu-delphi/micom-tooling-workshop-2025-dependencies).

## Installation

You should install `{InsightNetApr26}` like so:

``` r
install.packages("pak")
pak::pkg_install("cmu-delphi/InsightNet-apr-2026/InsightNetApr26", dependencies = TRUE)
```

## Verifying set up

Installing this package should install the correct versions for all the
packages needed to build the materials. This is actually a bit more than
you need to *run* the materials.

There are functions to check that all the dependencies are installed and to
perform the installation of any missing ones.

``` r
library(InsightNetApr26)
verify_setup()
#> ✔ You should be good to go!
```
