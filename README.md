
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
a bounded memory [algorithm](#acknowledgements) enabling:

  - Out-of-memory (OOM) processing capable of fitting **billions** of
    observations
  - *Fast* in-memory processing that requires less resources than `lm()`
    and `glm()`

`ploom` contains functions for iterative processing of in-memory data,
data in databases, and data on disk. `ploom` models are compatible with
`stats` summary functions, `tidy()` and `glance()`, and implement
`predict()` and `resid()` functions for in-memory and OOM data sources.

**See More:**  
[Usage]() and [Vignette]() for a more detailed capability overview  
[Benchmarking]() for comparisons with other commonly used packages

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

#### Fitting over Chunks

Models can be be fit with repeated calls to `fit()` over chunks of data.
Each call to `fit()` only needs to allocate memory for the provided
chunk, thereby bounding the required memory to chunk size.

The function `oomdata_tbl()` enables iteration over an in-memory
`tibble()` or `data.frame()`. When provided to `fit()`, all chunks will
iterated over.

``` r
chunks <- oomdata_tbl(mtcars, chunk_size = 16)
y <- fit(oomlm(mpg ~ wt + qsec + factor(am)), chunks)

glance(y)
```

    ## # A tibble: 1 x 10
    ##   r.squared adj.r.squared sigma statistic  p.value logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>    <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.850         0.834  2.46      52.7 1.21e-11  -72.1  154.  161.
    ## # … with 2 more variables: deviance <dbl>, df.residual <dbl>

#### Working with Databases

The function `oomdata_dbi()` enables iteratation over a [`DBI`]() result
set. `fit()` will automatically fit the model over all chunks.

``` r
# connect to database
con    <- DBI::dbConnect(RPostgres::Postgres(), dbname="mtcars")
result <- DBI::dbSendQuery(con, "select mpg, wt, qsec, am from mtcars;")
chunks <- oomdata_dbi(result, chunk_size = 16)

# fit model to all chunks
y <- fit(oomlm(mpg ~ wt + qsec + factor(am)), chunks)
summary(y)
```

    ## 
    ## Call:  oomlm(mpg ~ wt + qsec + factor(am))
    ## 
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   9.6178     6.9596   1.382 0.177915    
    ## wt           -3.9165     0.7112  -5.507 6.95e-06 ***
    ## qsec          1.2259     0.2887   4.247 0.000216 ***
    ## factor(am)1   2.9358     1.4109   2.081 0.046716 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Observations included:  32 
    ## Residual standard error: 2.459 on 28 degrees of freedom
    ## Multiple R-squared:  0.8497, Adjusted R-squared:  0.8336 
    ## F-statistic: 52.75 on 3 and 28 DF,  p-value: 1.21e-11

See the articles [NA]() and [NA]() for more on interfacing with
databases.

#### Prediction & Residuals

Prediction with `ploom` models is performed with the `predict()`
function. `predict()` provides options for confidence intervals,
prediction intervals, and standard error in addition to fit.

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
accessible on demand with `residuals()`:

``` r
sum(residuals(y, data = mtcars)^2)
```

    ## [1] 169.2859

Similarly we can use `augment()` but we must provide data.

``` r
augment(y, data = mtcars, std_error = TRUE)
```

    ## # A tibble: 32 x 10
    ##    .rownames   mpg    wt  qsec `factor(am)` .pred .resid .std_error
    ##    <chr>     <dbl> <dbl> <dbl> <fct>        <dbl>  <dbl>      <dbl>
    ##  1 Mazda RX4  21    2.62  16.5 1             22.5 -1.47       0.720
    ##  2 Mazda RX…  21    2.88  17.0 1             22.2 -1.16       0.744
    ##  3 Datsun 7…  22.8  2.32  18.6 1             26.3 -3.48       0.760
    ##  4 Hornet 4…  21.4  3.22  19.4 0             20.9  0.543      0.685
    ##  5 Hornet S…  18.7  3.44  17.0 0             17.0  1.69       0.749
    ##  6 Valiant    18.1  3.46  20.2 0             20.9 -2.75       0.768
    ##  7 Duster 3…  14.3  3.57  15.8 0             15.1 -0.754      0.942
    ##  8 Merc 240D  24.4  3.19  20   0             21.6  2.76       0.747
    ##  9 Merc 230   22.8  3.15  22.9 0             25.4 -2.55       1.34 
    ## 10 Merc 280   19.2  3.44  18.3 0             18.6  0.621      0.605
    ## # … with 22 more rows, and 2 more variables: .pred_lower <dbl>,
    ## #   .pred_upper <dbl>

## Alternatives

  - [`biglm`](https://cran.r-project.org/web/packages/biglm/index.html)
  - [`speedglm`](https://cran.r-project.org/web/packages/speedglm/index.html)

## Acknowledgements

Thanks to:

  - Fitting is performed via Alan Miller’s AS274 updating QR
    factorization algorithm
  - [`biglm`](https://cran.r-project.org/web/packages/biglm/index.html)
