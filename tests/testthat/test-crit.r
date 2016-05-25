context("Critical value calculation")

set.seed(17291)
x1 <- rnorm(100)
x2 <- rnorm(100)
yl <- rnorm(100)
yu <- yl + rexp(100)

fit <- lm_bounds(yl + yu ~ x1 + x2, boot = 100)

test_that("number and name indexing are equivalent", {
    expect_equal(calculate_crit(fit, term = 2, level = 0.87),
                 calculate_crit(fit, term = "x1", level = 0.87))
})
