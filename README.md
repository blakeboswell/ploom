
# ploom

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/ploom)](https://cran.r-project.org/package=ploom)
[![Build
Status](https://api.travis-ci.com/blakeboswell/ploom.svg?branch=develop)](https://api.travis-ci.com/blakeboswell/ploom)
[![Coverage
status](https://codecov.io/gh/blakeboswell/ploom/branch/develop/graph/badge.svg)](https://codecov.io/github/blakeboswell/ploom?branch=develop)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- [AppVeyor Build Status]() -->

## Overview

**Memory efficient** Linear and Generalized Linear models with support
for inference

  - Performs out-of-memory processing capable of fitting **billions** of
    observations
  - Performs in-memory processing at runtimes comparable to `lm()` and
    `glm()` with bounded memory
  - Manages iterative processing of data in a Database, file connection,
    or in-memory source

> `ploom` is currently in beta. See development \[roadmap\] for details.

## Installation

``` r
# development version from GitHub:
# install.packages("devtools")
devtools::install_github("blakeboswell/ploom")
```

## Usage

### In-memory Data

`oomlm()` models are intialized with a `formula()`; fit to data with
calls to `update()`; and analyzed with standard summary and extractor
functions.

``` r
library(ploom)

x <- oomlm(mpg ~ cyl + disp)
x <- update(x, data = mtcars)

tidy(x)
```

    ## # A tibble: 3 x 7
    ##   term        estimate std.error statistic  p.value conf.low  conf.high
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>      <dbl>
    ## 1 (Intercept)  34.7       2.55       13.6  4.02e-14  29.5     39.9     
    ## 2 cyl          -1.59      0.712      -2.23 3.37e- 2  -3.04    -0.131   
    ## 3 disp         -0.0206    0.0103     -2.01 5.42e- 2  -0.0416   0.000395

Because `update()` only allocates memory for the provided `data`,
fitting with more calls to `update()` on smaller *chunks* of `data`
requires less memory. For example, the following loop produces the same
fit while using only ~3% of the memory.

``` r
y  <- oomlm(mpg ~ cyl + disp)

for(i in 1:nrow(mtcars)) {
  y <- update(y, mtcars[i, ])
}

all.equal(x, y)
```

    ## [1] TRUE

Its possible to chunk and fit in-memory data without writing custom
loops using the function `oomdata_tbl()`. The following example is
equivalent to the previous in both fit results and total memory
allocated.

``` r
chunks <- oomdata_tbl(mtcars, chunk_size = 1)

y <- oomlm(mpg ~ cyl + disp)
y <- update(y, data = chunks)

all.equal(x, y)
```

    ## [1] TRUE

Fitting with more calls to `update()` does not result in a runtime
increase and frees RAM for simultaneous or parallel fitting. This is
ideal in multi-user environments and under heavy regression loads that
involve fitting multiple models. See
[benchmarking]('/articles/benchmark.html) for a detailed runtime
comparison with `lm()`, `glm()`.

### Out-of-memory Data

The pattern for fitting `oomlm()` models to out-of-memory data is the
same as in-memory data, except that the data to be fit is read between
calls to `update()`.

The function `oomdata_con()` assist with fitting fron file or gzip
connections, and `oomdata_dbi()` facilitaties fitting to data stored in
any database with a `DBI` backend.

``` r
library(RSQLite)
library(dbplyr)
library(dplyr)

con <- dbConnect(SQLite(), path = ":dbname:")
copy_to(con, mtcars, "mtcars", temporary = FALSE)
rs     <- dbSendQuery(con, "select mpg, cyl, disp from mtcars")

#' fit model
chunks <- oomdata_dbi(rs, chunk_size = 1)
z      <- update(oomlm(mpg ~ cyl + disp), data = chunks)

all.equal(x, z)
```

    ## [1] TRUE

See the articles [NA]() and [NA]() for more on interfacing with
databases.

### Inference and Prediction

`ploom()` models are compatible with dozens of `stats` `extractor`
functions and capable of providing standard errors, robust standard
errors, confidence intervals, prediction intervals, and
    residuals.

## Alternatives

  - [`biglm`](https://cran.r-project.org/web/packages/biglm/index.html)
  - [`speedglm`](https://cran.r-project.org/web/packages/speedglm/index.html)

## Acknowledgements

Thanks to:

  - Fitting is performed via Alan Miller’s AS274 updating QR
    factorization algorithm
  - [`biglm`](https://cran.r-project.org/web/packages/biglm/index.html)
