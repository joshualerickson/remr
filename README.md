
<!-- README.md is generated from README.Rmd. Please edit that file -->

# remr

<!-- badges: start -->

[![R-CMD-check](https://github.com/joshualerickson/remr/workflows/R-CMD-check/badge.svg)](https://github.com/joshualerickson/remr/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `remr` is to get transects along a linestring so that you
can then perform a Relative Elevation Model.

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("joshualerickson/remr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(remr)
line <- system.file('inst', 'bg_line.shp', package = 'remr')
ele <- system.file('inst', 'bg_ele.tif', package = 'remr')

rem <- get_transects(line, ele, distance = 100, length = 500)

ele_crop <- terra::crop(ele, terra::vect(sf::st_buffer(line, 200)))
terra::plot(ele_crop)
plot(line$geometry, add = TRUE)
plot(rem$geometry, add = TRUE)
```