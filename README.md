
<!-- README.md is generated from README.Rmd. Please edit that file -->

# traceShiny

<!-- badges: start -->
<!-- badges: end -->

This is the web-server version of our R package, TRACE (Tandem Repeat
Analysis from Capillary Electrophoresis), please visit the TRACE Github
page for more [details](https://github.com/zachariahmclean/trace).

## Web Server Version

traceShiny is available as a web server under
<https://traceshiny.mgh.harvard.edu>.

## Installation

You can install the R package version of traceShiny from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("zachariahmclean/traceShiny")
```

## Run traceShiny locally

``` r
library(traceShiny)
devtools::load_all()
traceShiny::run_traceShiny()
```
