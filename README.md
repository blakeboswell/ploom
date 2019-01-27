
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
prediction intervals, and standard error in addition to
fit.

``` r
yhat <- predict(y, new_data = mtcars, se_fit = TRUE, interval = "prediction")
head(yhat[["fit"]])
```

    ##                      .pred      lwr      upr
    ## Mazda RX4         22.47046 17.22244 27.71849
    ## Mazda RX4 Wag     22.15825 16.89630 27.42019
    ## Datsun 710        26.28107 21.00938 31.55275
    ## Hornet 4 Drive    20.85744 15.62898 26.08590
    ## Hornet Sportabout 17.00959 11.74463 22.27455
    ## Valiant           20.85409 15.57760 26.13058

`ploom` models do not store the residuals during fitting.

``` r
u <- oomlm_residuals(y, data = mtcars)
sum(u^2)
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
