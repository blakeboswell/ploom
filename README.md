
# ploom

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/ploom)](https://cran.r-project.org/package=ploom)
[![Build
Status](https://api.travis-ci.com/blakeboswell/ploom.svg?branch=develop)](https://api.travis-ci.com/blakeboswell/ploom)
[![Coverage
status](https://codecov.io/gh/blakeboswell/ploom/branch/develop/graph/badge.svg)](https://codecov.io/github/blakeboswell/ploom?branch=develop)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

<!-- [AppVeyor Build Status]() -->

## Overview

`ploom` provides tools for **memory efficient** fitting of Linear and
Generalized Linear models. `ploom` fits models with a [bounded
memory](#aknowledgements) aglorithm enabling:

  - **Out-of-memory** processing capable of fitting billions of
    observations
  - **Bounded in-memory** processing with runtimes comparable to `lm()`
    and `glm()`

`ploom` models are compatible with many [`stats`]() and [`tidy`]()
statistical analysis functions. They are capable of generating robust
standard errors, confidence and prediction intervals, residuals, and
other artifacts for inferential analysis.

## Installation

``` r
# development version from GitHub:
# install.packages("devtools")
devtools::install_github("blakeboswell/ploom")
```

## Usage

Models are intialized with a `formula()`; fit to data with calls to
`fit()`; and summarized with standard functions such as `tidy()`,
`glance()`, and `summary()`.

``` r
library(ploom)

y <- oomlm(mpg ~ cyl + disp)
y <- fit(y, data = mtcars)

tidy(y)
```

    ## # A tibble: 3 x 7
    ##   term        estimate std.error statistic  p.value conf.low  conf.high
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>      <dbl>
    ## 1 (Intercept)  34.7       2.55       13.6  4.02e-14  29.5     39.9     
    ## 2 cyl          -1.59      0.712      -2.23 3.37e- 2  -3.04    -0.131   
    ## 3 disp         -0.0206    0.0103     -2.01 5.42e- 2  -0.0416   0.000395

#### Fitting in Chunks

Models can be be fit with repeated calls to `fit()` over chunks of data.
Each call to `fit()` only needs to allocate memory for the provided
chunk, thereby bounding the fitting process to chunk size.

The function `oomdata_tbl()` enables iteration over an in-memory
`tibble()` or `data.frame()` object.

``` r
chunks <- oomdata_tbl(mtcars, chunk_size = 16)

while((!is.null(chunk <- chunks()))) {
  print(nrow(chunk))
}
```

    ## [1] 16
    ## [1] 16

When provided to `fit()`, all chunks will be iterated over and fit.

``` r
y <- fit(oomlm(mpg ~ cyl + disp), chunks)

# glance(y)
```

#### Working with Databases

Similarly, `oomdata_dbi()` facilitaties fitting to data in a database.
It is compatible with any database having a `DBI` backend.

``` r
#' connect to database
con     <- DBI::dbConnect(RPostgres::Postgres(), dbname = "mtcars")
result  <- DBI::dbSendQuery(con, "select mpg, cyl, disp from mtcars;")

#' fit model
X <- oomdata_dbi(result, chunk_size = 1)
y <- fit(oomlm(mpg ~ cyl + disp), X)
```

See the articles [NA]() and [NA]() for more on interfacing with
databases.

#### Inference and Prediction

## Alternatives

  - [`biglm`](https://cran.r-project.org/web/packages/biglm/index.html)
  - [`speedglm`](https://cran.r-project.org/web/packages/speedglm/index.html)

## Acknowledgements

Thanks to:

  - Fitting is performed via Alan Miller’s AS274 updating QR
    factorization algorithm
  - [`biglm`](https://cran.r-project.org/web/packages/biglm/index.html)
