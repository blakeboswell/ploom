
# ploom

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/ploom)](https://cran.r-project.org/package=ploom)
[![Build
Status](https://api.travis-ci.com/blakeboswell/ploom.svg?branch=develop)](https://api.travis-ci.com/blakeboswell/ploom)
[![Coverage
status](https://codecov.io/gh/blakeboswell/ploom/branch/develop/graph/badge.svg)](https://codecov.io/github/blakeboswell/ploom?branch=develop)

<!-- [AppVeyor Build Status]() -->

<!-- [Coverage Status]() -->

## Overview

A collection of tools for **out-of-memory** and **memory efficient**
linear model fitting with support for inference. Provides `oomlm()` and
`oomglm()` functions for linear and genearlised linear modeling. Models
are implemented using Alan Miller’s AS274 updating QR factorization
algorithm which enables models with `p` variables to be fit in `p^2`
memory.

  - Out-of-memory procesing capable of fitting billions of observations
  - In-memory runtimes at least as good as `lm()` and `glm()`
  - Efficient memory usage ideal for multi-user environments with heavy
    regression loads

> `ploom` is in beta. See [roadmap]() for details.

## Features

  - Linear and Generalized Linear Models with robust standard errors
  - Data streaming functions from Database and file connections as well
    as chunking of in-memory sources
  - Inferential statistics such as standard errors, prediction
    intervals, and confidence intervals

> The beta version of `ploom` has essentially the same features as
> [`biglm`](https://cran.r-project.org/web/packages/biglm/index.html)
> with a slight runtime improvement. More differentiating features are
> under development.

## Installation

``` r
# the early development version from GitHub:
# install.packages("devtools")
devtools::install_github("blakeboswell/ploom")
```

## Usage

Resource efficient in-memory linear and generalized linear models.

``` r
library(ploom)

chunks  <- oomdata_tbl(mtcars, chunk_size = 1)

x <- oomlm(mpg ~ cyl + disp, data = chunks)
tidy(x)
```

    ## # A tibble: 3 x 7
    ##   term        estimate std.error statistic  p.value conf.low  conf.high
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>      <dbl>
    ## 1 (Intercept)  34.7       2.55       13.6  4.02e-14  29.5     39.9     
    ## 2 cyl          -1.59      0.712      -2.23 3.37e- 2  -3.04    -0.131   
    ## 3 disp         -0.0206    0.0103     -2.01 5.42e- 2  -0.0416   0.000395

``` r
y <- iter_weight(oomglm(mpg ~ cyl + disp), data = chunks)
tidy(y)
```

    ## # A tibble: 3 x 7
    ##   term        estimate std.error statistic  p.value conf.low  conf.high
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>      <dbl>
    ## 1 (Intercept)  34.7       2.55       13.6  4.02e-14  29.5     39.9     
    ## 2 cyl          -1.59      0.712      -2.23 3.37e- 2  -3.04    -0.131   
    ## 3 disp         -0.0206    0.0103     -2.01 5.42e- 2  -0.0416   0.000395

Out-of-memory model fitting for data in a Database or on disk.

``` r
library(RPostgres)

con    <- dbConnect(drv = Postgres(), dbname = "mtcars")
query  <- "select mpg, cyl, disp from mtcars;"
rs     <- dbSendQuery(con, query)

chunks <- oomdata_dbi(rs, chunk_size = 4)
y <- iter_weight(oomglm(mpg ~ cyl + disp), data = chunks)
```

## Alternatives

[`biglm`](https://cran.r-project.org/web/packages/biglm/index.html)
[`speedglm`](https://cran.r-project.org/web/packages/speedglm/index.html)

## Acknowledgements

Thanks to:

  - [`biglm`](https://cran.r-project.org/web/packages/biglm/index.html)
