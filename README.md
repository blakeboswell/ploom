
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

The core of `ploom` consists of the updating Linear Models `oomlm` and
`oomglm` and the data reading and writing objects, `streams`, for
feeding observations into the models and online logging of model
statistics.

### `oomlm` and `oomglm`

``` r
library(ploom)

chunks <- purrr::map(1:nrow(mtcars), function(i) mtcars[i, ])
```

The functions `oomlm` and `oomglm` are the analogs of base R `lm` and
`glm`.

Initialize a new `oomlm` with data, and then loop update. …

``` r
x  <- oomlm(chunks[[1]], mpg ~ cyl + disp + hp + wt)
for(chunk in chunks[2:length(chunks)]) {
  x <- update_oomlm(chunk, x)
}
```

Or initialize with formula only, and then loop to update. …

``` r
y  <- oomlm(mpg ~ cyl + disp + hp + wt)
for(chunk in chunks) {
  y <- update_oomlm(chunk, y)
}
```

Or use `reduce` to initialize and update.

``` r
z <- purrr::reduce(chunks, update_oomlm,
                   .init = oomlm(mpg ~ cyl + disp + hp + wt))
```

All approaches produce the same result.

``` r
summary(z)
```

    ## 
    ## Out-of-memory Linear Model:
    ## oomlm(`data`, formula = mpg ~ cyl + disp + hp + wt)
    ## 
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 40.82854    2.75747  14.807 1.76e-14 ***
    ## cyl         -1.29332    0.65588  -1.972 0.058947 .  
    ## disp         0.01160    0.01173   0.989 0.331386    
    ## hp          -0.02054    0.01215  -1.691 0.102379    
    ## wt          -3.85390    1.01547  -3.795 0.000759 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.513 on 27 degrees of freedom
    ## Multiple R-squared:  0.8486, Adjusted R-squared:  0.8262 
    ## F-statistic: 37.84 on 4 and 27 DF,  p-value: 1.061e-10

### `streams`

## Alternatives

> forthcoming

## Acknowledgements

> forthcoming
