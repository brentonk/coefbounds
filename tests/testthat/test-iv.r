context("Instrumental variables estimation")
library("AER")

test_that("Underidentified models not allowed", {
    set.seed(876)
    x1 <- rnorm(10)
    x2 <- rnorm(10)
    z1 <- rnorm(10)
    yl <- yh <- rnorm(10)

    expect_error(coefbounds(yl + yh ~ x1 + x2 | z1,
                            boot = 0),
                 "fewer instruments")
})

test_that("No IVs allowed in logit", {
    set.seed(21)
    x1 <- rnorm(10)
    z1 <- rnorm(10)
    yl <- rbinom(10, 1, 0.5)
    yh <- yl

    expect_error(coefbounds(yl + yh ~ x1 | z1,
                            model = "logit",
                            boot = 0),
                 "instrumental variables not allowed")
})

test_that("coefbounds() equals ivreg() when point-identified", {
    set.seed(47)
    yl <- rnorm(100)
    yu <- yl
    x1 <- rnorm(100)
    x2 <- rnorm(100)
    x3 <- rnorm(100)
    z1 <- 0.5 * x1 + 0.5 * rnorm(100)

    fit_ivreg <- ivreg(yl ~ x1 + x2 + x3 | z1 + x2 + x3)
    fit_bd <- coefbounds(yl + yu ~ x1 + x2 + x3 | z1 + x2 + x3,
                         boot = 0)

    expect_equal(coef(fit_ivreg),
                 coef(fit_bd)[, 1])
    expect_equal(coef(fit_ivreg),
                 coef(fit_bd)[, 2])
})
