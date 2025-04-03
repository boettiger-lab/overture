
<!-- README.md is generated from README.Rmd. Please edit that file -->

# overture

<!-- badges: start -->
[![R-CMD-check](https://github.com/boettiger-lab/overture/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/boettiger-lab/overture/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Easily access spatial vector data from [Overture
Maps](https://overturemaps.org/) with duckdb.

## Installation

You can install the development version of overture from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("boettiger-lab/overture")
```

## Getting Started

``` r
library(overture)

df <- overture("divisions", "division_area")
df
```
