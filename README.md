
# ploom

<!-- [CRAN_Status_Badge]() -->

<!-- [Build Status]() -->

<!-- [AppVeyor Build Status]() -->

<!-- [Coverage Status]() -->

## Overview

A collection of tools for out-of-memory linear model fitting and
inference. Implements `lm` and `glm` analogs using Alan Miller’s AS274
updating QR factorization algorithm. Collects and reports an array of
pertinent fit statistics. Provides flexible and easy to use mechanisms
to stream in data and stream out results during fitting.

> Currently in early development stage.

### tl;dr Features

> forthcoming

## Installation

``` r
# the early development version from GitHub:
# install.packages("devtools")
devtools::install_github("bboswell/ploom")
```

## Usage

### Model Initializing and Updating

The functions `oomlm` and `oomglm` are similar to base `lm` and `glm`
for fitting in-memory data.

``` r
w <- oomlm(mpg ~ cyl + disp, data = mtcars)
```

Models are initalized with a call to `oomlm` and updated with
`update_oomlm`. The recommended pattern is to initialize models without
referencing the data, then call `update_oomlm` on each data chunk in the
exact same way.

``` r
# proxy for big data feed (`purrr::pmap`)
chunks  <- pmap(mtcars, list)

# initialize the model
x <- oomlm(mpg ~ cyl + disp)

# iteratively update model with data chunks
for(chunk in chunks) {
  update_oomlm(x, chunk)
}
```

Separating model initialization and processing of the first data chunk
enables functional patterns like `reduce` to take the place of loops.
The below example is equivalent to the above `for` loop.

``` r
# avoid loops altogether with `purrr::reduce`
y <- reduce(chunks, update_oomlm, .init = oomlm(mpg ~ cyl + disp))
```

For maximum flexibility, `ploom` also supports providing data on
initialization similar to [`biglm`](https://github.com/cran/biglm).

``` r
# initial fit
z  <- oomlm(mpg ~ cyl + disp, chunks[[1]])

# iteratively update model with additional data chunks
for(chunk in tail(chunks, -1)) {
  z <- update_oomlm(x, data = chunk)
}
```

### Using Feeds for a Variety of OOM Data Formats

## Alternatives

> forthcoming

## Acknowledgements

> forthcoming
