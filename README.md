
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

#### `oomlm` and `oomglm`

The linear modeling functions `oomlm` and `oomglm` are the updating
analog of base R `lm` and `glm`.

``` r
library(ploom)
```

``` r
mdl <- ploom::oomlm(mtcars[1, ], mpg ~ cyl + disp + hp + wt)
purrr::walk(2:nrow(mtcars), ~update_oomlm(mtcars[., ], mdl))

summary(mdl)
```

    ## 
    ## Out-of-memory Linear Model:
    ## ploom::oomlm(mtcars[1, ], mpg ~ cyl + disp + hp + wt)
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

#### `streams`

## Alternatives

> forthcoming

## Acknowledgements

> forthcoming
