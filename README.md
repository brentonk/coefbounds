
<!-- README.md is generated from README.Rmd. Please edit that file -->
coefbounds
==========

Nonparametric Bounds for Regression with Interval-Censored Outcomes

Author: Brenton Kenkel, Vanderbilt University

Installation
------------

**coefbounds** is not yet available on CRAN. To install directly from GitHub:

``` r
if (!require("devtools")) {
    install.packages("devtools")
    library("devtools")
}

devtools::install_github("brentonk/coefbounds")
```

Note
----

**coefbounds** is in an early stage of development. Backward-incompatible API changes are possible until version 1.0.0 is released.

Usage
-----

Below are some simple usage examples. For more fleshed-out examples, see the package vignette: `vignette("introduction", package = "coefbounds")`.

``` r
library("coefbounds")
```

I'll begin by generating some simple data with an interval-censored outcome.

``` r
set.seed(97)
x1 <- rnorm(100)
x2 <- rnorm(100)
y <- 1 - x1 + x2 + rnorm(100)
lwr <- floor(y)
upr <- ceiling(y)
```

To estimate coefficient bounds, use `lm_bounds()` with a formula of the form `yl + yu ~ x1 + x2 + ...`, where `yl` is the lower bound on each response value and `yu` is the upper bound.

``` r
fit_full <- lm_bounds(lwr + upr ~ x1 + x2, boot = 100)
fit_full
#> 
#> Call:
#> lm_bounds(formula = lwr + upr ~ x1 + x2, boot = 100)
#> 
#> Estimated coefficient bounds:
#>              lower upper
#> (Intercept)  0.652  1.65
#> x1          -1.247 -0.40
#> x2           0.722  1.48
```

For inference, make sure `lm_bounds()` is run with `boot > 0` and use `interval_hypothesis()` or `confint()`.

``` r
interval_hypothesis(fit = fit_full,
                    term = "x1",
                    interval = c(0, 0),
                    type = "subset")
#> 
#> Null hypothesis: identification region for x1 contains [0, 0]
#> Estimated identification region: [-1.247, -0.405]
#> Test statistic: sqrt(N) * directed Hausdorff distance
#> 
#> stat = 4.05, p-value = <0.01
interval_hypothesis(fit = fit_full,
                    term = "x2",
                    interval = c(0.7, 1.6),
                    type = "equal")
#> 
#> Null hypothesis: identification region for x2 equals [0.7, 1.6]
#> Estimated identification region: [0.722, 1.480]
#> Test statistic: sqrt(N) * Hausdorff distance
#> 
#> stat = 1.2, p-value = 0.31
```

``` r
confint(fit_full, level = 0.99)
#>                0.5 %   99.5 %
#> (Intercept)  0.40717  1.89768
#> x1          -1.47298 -0.17848
#> x2           0.47613  1.72565
```

To Do
-----

-   Allow clustering in critical value bootstrap.
-   Enable parallel processing in bootstrap.
