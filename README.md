
# ploom

<!-- [CRAN_Status_Badge]() -->

<!-- [Build Status]() -->

<!-- [AppVeyor Build Status]() -->

<!-- [Coverage Status]() -->

# Overview

A collection of tools for out-of-memory linear model fitting and
inference. Implements `lm` and `glm` analogs using Alan Miller’s AS274
updating QR factorization algorithm. Collects and reports an array of
pertinent fit statistics. Provides flexible and easy to use mechanisms
to stream in data and stream out results during fitting.

> Currently in early development stage.

## tl;dr Features

> forthcoming

# Installation

``` r
# the early development version from GitHub:
# install.packages("devtools")
devtools::install_github("blakeboswell/ploom")
```

# Usage

## Model Initializing and Updating

### Linear Models

The `ploom` linear model, `oomlm`, is similar to base `lm` for fitting
in-memory data.

``` r
x <- oomlm(mpg ~ cyl + disp, data = mtcars)
```

Models are initalized with a call to `oomlm` and updated with `update`.
The intended pattern is to initialize models without referencing data,
then call `update` on each data chunk.

``` r
# proxy for big data feed 
chunks  <- purrr::pmap(mtcars, list)

# initialize the model
x <- oomlm(mpg ~ cyl + disp)

# iteratively update model with data chunks
for(chunk in chunks) {
  x <- update(x, chunk)
}
```

We can avoid loops with functional patterns like `reduce`.

``` r
x <- purrr::reduce(chunks, update, .init = oomlm(mpg ~ cyl + disp))
```

### Generalized Linear Models

The `ploom::oomglm` function fits generalized linear models via
Iteratively Weighted Least Squares (IWLS).

When fitting in-memory data the process is similar to `oomlm` but we use
the function `reweight` instead of `update`. `reweight` fits the model
via iterative passes over the data until convergence.

``` r
# initialize the model
x <- oomglm(mpg ~ cyl + disp)

# re-weight 8 times or until convergence
x <- reweight(x, mtcars, max_iter = 8)
```

To fit data in chunks, use the `oomfeed` object:

``` r
# initialize the model
x    <- oomglm(mpg ~ cyl + disp)
feed <- oomfeed(mtcars, chunksize = 10)

# iteratively reweight model
x <- reweight(x, feed, max_iter = 8)
```

The `reweight` process can also be implemented directly with `ploom`
functions:

``` r
x    <- oomglm(mpg ~ cyl + disp)
feed <- oomfeed(mtcars, chunksize = 10)

# a first pass over the data
x <- init_update(x)
x <- update(x, feed)
x <- end_update(x)
x
```

    ## 
    ## Call:  oomglm(mpg ~ cyl + disp)
    ## 
    ## Coefficients:
    ## (Intercept)          cyl         disp  
    ##    34.66099     -1.58728     -0.02058  
    ## 
    ## Observations included:  32 
    ## Degrees of Freedom: 31 Total (i.e. Null);  29 Residual
    ## Residual Deviance: 270.7     AIC: 167.1 
    ## 
    ## Converged: FALSE 
    ## Number of Fisher Scoring iterations: 1

``` r
# a second pass over the data
x <- init_update(x)
x <- update(x, feed)
x <- end_update(x)
x
```

    ## 
    ## Call:  oomglm(mpg ~ cyl + disp)
    ## 
    ## Coefficients:
    ## (Intercept)          cyl         disp  
    ##    34.66099     -1.58728     -0.02058  
    ## 
    ## Observations included:  32 
    ## Degrees of Freedom: 31 Total (i.e. Null);  29 Residual
    ## Residual Deviance: 270.7     AIC: 167.1 
    ## 
    ## Converged: TRUE 
    ## Number of Fisher Scoring iterations: 2

This is useful when debugging / evaluating models with long runtimes by
exposing the individual steps of the model process for inspection.

## Using Feeds for a Variety of OOM Data Formats

# Alternatives

> forthcoming

# Acknowledgements

> forthcoming
