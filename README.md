
# ploom

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/ploom)](https://cran.r-project.org/package=ploom)
[![Build
Status](https://api.travis-ci.com/blakeboswell/ploom.svg?branch=develop)](https://api.travis-ci.com/blakeboswell/ploom)
[![Coverage
status](https://codecov.io/gh/blakeboswell/ploom/branch/develop/graph/badge.svg)](https://codecov.io/github/blakeboswell/ploom?branch=develop)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- [AppVeyor Build Status]() -->

## Overview

ploom provides tools for **memory efficient** fitting of Linear and
Generalized Linear models. Inspired by biglm, ploom fits models using a
bounded memory [algorithm](#acknowledgements) that enables:

  - Out-of-memory (OOM) processing capable of fitting **billions** of
    observations
  - *Fast* in-memory processing requiring less resources than `lm()` and
    `glm()`

ploom models are

  - Compatible with `tidy()`, `glance()`, `augment()` and many `stats`
    functions such as `predict()` and `residuals()`
  - Capable of iteratively processing data stored in-memory, in a
    database or on disk

## Installation

``` r
# development version from GitHub:
# install.packages("devtools")
devtools::install_github("blakeboswell/ploom")
```

## Usage

Models are intialized with a `formula`; fit to data with calls to
`fit()`; and summarized with standard functions such as `tidy()`,
`glance()`, and `summary()`.

``` r
library(ploom)

y <- oomlm(mpg ~ wt + qsec + factor(am))
y <- fit(y, data = mtcars)

tidy(y)
```

    ## # A tibble: 4 x 7
    ##   term        estimate std.error statistic    p.value conf.low conf.high
    ##   <chr>          <dbl>     <dbl>     <dbl>      <dbl>    <dbl>     <dbl>
    ## 1 (Intercept)     9.62     6.96       1.38 0.178       -4.64       23.9 
    ## 2 wt             -3.92     0.711     -5.51 0.00000695  -5.37       -2.46
    ## 3 qsec            1.23     0.289      4.25 0.000216     0.635       1.82
    ## 4 factor(am)1     2.94     1.41       2.08 0.0467       0.0457      5.83

### Bounded Memory

Models can be be fit with repeated calls to `fit()` over chunks of data.
Each call to `fit()` only needs to allocate memory for the provided
chunk, thereby bounding the required memory.

``` r
y <- oomlm(mpg ~ wt + qsec + factor(am))
y <- fit(y, mtcars[1:16, ])
y <- fit(y, mtcars[17:32, ])

coef(y)
```

    ## (Intercept)          wt        qsec factor(am)1 
    ##    9.617781   -3.916504    1.225886    2.935837

### Fitting over Chunks

The function `oomdata_tbl()` enables iteration over an in-memory
`tibble` or `data.frame`. When an `oomdata_tbl()` is provided as the
data argument to `fit()`, all chunks are automatically iterated over.

``` r
chunks <- oomdata_tbl(mtcars, chunk_size = 16)
fit(oomlm(mpg ~ wt + qsec + factor(am)), chunks)
```

    ## 
    ## Call:  oomlm(mpg ~ wt + qsec + factor(am))
    ## 
    ## Coefficients:
    ## (Intercept)           wt         qsec  factor(am)1  
    ##       9.618       -3.917        1.226        2.936  
    ## 
    ## Observations included:  32

### Working with Databases

The function `oomdata_dbi()` enables iteratation over a [`DBI`]() result
set. `fit()` will automatically fit the model over all chunks.

``` r
# connect to database
con    <- DBI::dbConnect(RPostgres::Postgres(), dbname="mtcars")
result <- DBI::dbSendQuery(con, "select mpg, wt, qsec, am from mtcars;")
chunks <- oomdata_dbi(result, chunk_size = 16)

# fit model to all chunks
y <- fit(oomlm(mpg ~ wt + qsec + factor(am)), chunks)

# inspect fit statistiscs
glance(y)
```

    ## # A tibble: 1 x 10
    ##   r.squared adj.r.squared sigma statistic  p.value logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.850         0.834  2.46      52.7 1.21e-11  -72.1  154.  161.
    ## # … with 2 more variables: deviance <dbl>, df.residual <dbl>

See the articles [NA]() and [NA]() for more on interfacing with
databases.

### Prediction & Residuals

Prediction with `ploom` models is performed with the `predict()`
function. `predict()` provides options for confidence intervals,
prediction intervals, and standard error in addition to fit. Because
ploom models do not store any data while fitting, we must also provide
data.

``` r
predict(y, new_data = mtcars, std_error = TRUE, interval = "prediction")
```

    ## # A tibble: 32 x 4
    ##    .pred .std_error .pred_lower .pred_upper
    ##    <dbl>      <dbl>       <dbl>       <dbl>
    ##  1  22.5      0.720       17.2         27.7
    ##  2  22.2      0.744       16.9         27.4
    ##  3  26.3      0.760       21.0         31.6
    ##  4  20.9      0.685       15.6         26.1
    ##  5  17.0      0.749       11.7         22.3
    ##  6  20.9      0.768       15.6         26.1
    ##  7  15.1      0.942        9.66        20.4
    ##  8  21.6      0.747       16.4         26.9
    ##  9  25.4      1.34        19.6         31.1
    ## 10  18.6      0.605       13.4         23.8
    ## # … with 22 more rows

`ploom` models do not store the residuals during fitting. Residuals are
accessible on demand with
    `residuals()`:

``` r
sum(residuals(y, data = mtcars)^2)
```

    ## [1] 169.2859

## Alternatives

  - [`biglm`](https://cran.r-project.org/web/packages/biglm/index.html)
  - [`speedglm`](https://cran.r-project.org/web/packages/speedglm/index.html)

## Acknowledgements

Thanks to:

  - Fitting is performed via Alan Miller’s AS274 updating QR
    factorization algorithm
  - [`biglm`](https://cran.r-project.org/web/packages/biglm/index.html)
