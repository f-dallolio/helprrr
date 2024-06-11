
# helprrr

<!-- badges: start -->
<!-- badges: end -->

The goal of helprrr is to provides helper and convenience functions for handling symbolic objects (symbols and calls), functions, strings, etc.

## Installation

You can install the development version of helprrr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("f-dallolio/helprrr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(helprrr)

encalls("mean", quote(mean), mean, .named = TRUE)
# $`mean()`
# mean()
# 
# $`mean()`
# mean()
# 
# $`base::mean(x, ...)`
# base::mean(x, ...)

symcs("mean", quote(mean(1:10)), mean, .named = TRUE)
# $mean
# mean
# 
# $`mean(1:10)`
# mean(1:10)
# 
# $`base::mean(x, ...)`
# base::mean(x, ...)
```

