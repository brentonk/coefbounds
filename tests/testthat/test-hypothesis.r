context("Interval hypothesis testing")

set.seed(1776)
x1 <- rnorm(100)
x2 <- rnorm(100)
yl <- rnorm(100)
yu <- yl + rexp(100)

fit <- lm_bounds(yl + yu ~ x1 + x2, boot = 111)

test_that("interval_hypothesis() fails on bad input", {
    fit_bad <- lm_bounds(yl + yu ~ x1 + x2, boot = 0)
    expect_error(interval_hypothesis(fit_bad,
                                     term = "x1",
                                     interval = c(0, 0)),
                 "no bootstrap")

    expect_error(interval_hypothesis(fit,
                                     term = c("x1", "x2"),
                                     interval = c(0, 0)),
                 "multiple terms")

    expect_error(interval_hypothesis(fit,
                                     term = "x3",
                                     interval = c(0, 0)),
                 "not in the model")

    expect_error(interval_hypothesis(fit,
                                     term = "x1",
                                     interval = 0:2),
                 "length 2")

    expect_error(interval_hypothesis(fit,
                                     term = "x1",
                                     interval = 1:0),
                 "exceeds upper bound")
})

test_that("interval_hypothesis() and confint() yield consistent results", {
    ## Directed hypothesis
    hyp_x1 <- confint(fit,
                      parm = "x1",
                      level = 0.95,
                      type = "DU")
    test_x1 <- interval_hypothesis(fit,
                                   term = "x1",
                                   interval = hyp_x1,
                                   type = "subset")
    expect_true(abs(test_x1$p - 0.05) < 1 / test_x1$n_boot)

    ## Undirected hypothesis
    hyp_x2 <- confint(fit,
                      parm = "x2",
                      level = 0.90,
                      type = "CC")
    hyp_x2 <- c(hyp_x2[1, 1], hyp_x2[2, 2])
    test_x2 <- interval_hypothesis(fit,
                                   term = "x2",
                                   interval = hyp_x2,
                                   type = "equal")
    expect_true(abs(test_x2$p - 0.10) < 1 / test_x2$n_boot)
})
