
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
Generalized Linear models. Inspired by `biglm`, `ploom` fits models with
a bounded memory [**algorithm**](#acknowledgements) enabling:

  - Out-of-memory (OOM) processing capable of fitting **billions** of
    observations
  - *Fast* in-memory processing that requires less resources than `lm()`
    and `glm()`

`ploom` contains functions for iterative processing of in-memory data,
data in databases, and data on disk. `ploom` models are compatible with
`stats` summary functions, `tidy()` and `glance()`, and implement
`predict()` and `resid()` functions for in-memory and OOM data sources.

**See More:**  
[**Usage**]() and [**Vignette**]() for a more detailed capability
overview  
[**Benchmarking**]() for comparisons with other commonly used packages

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

# define model, fit to data, and summarize
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

#### Fitting over Chunks

Models can be be fit with repeated calls to `fit()` over chunks of data.
Each call to `fit()` only needs to allocate memory for the provided
chunk, thereby bounding the required memory to chunk size.

The function `oomdata_tbl()` enables iteration over an in-memory
`tibble()` or `data.frame()`.

``` r
chunks <- oomdata_tbl(mtcars, chunk_size = 16)

# iterate over all observations in `chunks()`
while((!is.null(chunk <- chunks()))) {
  print(nrow(chunk))
}
```

    ## [1] 16
    ## [1] 16

`fit()` will automatically fit the model over all chunks.

``` r
# fit model to all chunks
y <- fit(oomlm(mpg ~ cyl + disp), chunks)
glance(y)
```

    ## # A tibble: 1 x 10
    ##   r.squared adj.r.squared sigma statistic p.value logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>   <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.760         0.743  3.06      45.8 1.06e-9  -79.6  167.  173.
    ## # … with 2 more variables: deviance <dbl>, df.residual <dbl>

#### Working with Databases

The function `oomdata_dbi()` enables iteratation over a [`DBI`]() result
set. `fit()` will automatically fit the model over all chunks.

``` r
# connect to database
con    <- DBI::dbConnect(RPostgres::Postgres(), dbname="mtcars")
result <- DBI::dbSendQuery(con, "select mpg, cyl, disp from mtcars;")
chunks <- oomdata_dbi(result, chunk_size = 16)

# fit model to all chunks
y <- fit(oomlm(mpg ~ cyl + disp), chunks)
summary(y)
```

    ## 
    ## Call:  oomlm(mpg ~ cyl + disp)
    ## 
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 34.66099    2.54700  13.609 4.02e-14 ***
    ## cyl         -1.58728    0.71184  -2.230   0.0337 *  
    ## disp        -0.02058    0.01026  -2.007   0.0542 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Observations included:  32 
    ## Residual standard error: 3.055 on 29 degrees of freedom
    ## Multiple R-squared:  0.7596, Adjusted R-squared:  0.743 
    ## F-statistic: 45.81 on 2 and 29 DF,  p-value: 1.058e-09

See the articles [NA]() and [NA]() for more on interfacing with
databases.

#### Prediction & Residuals

Prediction with `ploom` models is performed with the `predict()`
function. `predict()` provides options for confidence intervals,
prediction intervals, and standard error in addition to
fit.

``` r
yhat <- predict(y, new_data = mtcars, se_fit = TRUE, interval = "prediction")
head(yhat[["fit"]])
```

    ##                      .pred       lwr      upr
    ## Mazda RX4         21.84395 15.377338 28.31057
    ## Mazda RX4 Wag     21.84395 15.377338 28.31057
    ## Datsun 710        26.08886 19.588791 32.58892
    ## Hornet 4 Drive    19.82676 13.427084 26.22643
    ## Hornet Sportabout 14.55267  8.096698 21.00865
    ## Valiant           20.50602 14.157647 26.85439

`ploom` models do not store the residuals during
    fitting.

## Alternatives

  - [`biglm`](https://cran.r-project.org/web/packages/biglm/index.html)
  - [`speedglm`](https://cran.r-project.org/web/packages/speedglm/index.html)

## Acknowledgements

Thanks to:

  - Fitting is performed via Alan Miller’s AS274 updating QR
    factorization algorithm
  - [`biglm`](https://cran.r-project.org/web/packages/biglm/index.html)
