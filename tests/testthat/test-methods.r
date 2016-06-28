context("S3 methods")

test_that("nobs() method works", {
    set.seed(5301)
    x1 <- rnorm(10)
    yl <- yh <- rnorm(10)
    cl <- c(rep(1, 6), rep(2, 4))

    fit <- coefbounds(yl + yh ~ x1,
                      boot = 0,
                      cluster_id = cl)

    expect_equal(fit$nobs, c(fit = 10, cluster = 2))
    expect_equal(nobs(fit), 10)
    expect_equal(nobs(fit, type = "cluster"), 2)

    fit_no <- coefbounds(yl + yh ~ x1,
                         boot = 0)

    expect_equal(fit_no$nobs, c(fit = 10, cluster = 10))
    expect_equal(nobs(fit_no), 10)
    expect_equal(nobs(fit_no, type = "cluster"), 10)
})
