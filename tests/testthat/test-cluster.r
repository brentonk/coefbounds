context("Cluster bootstrap")

test_that("cluster_id must have the right number of observations", {
    set.seed(71)
    x1 <- rnorm(10)
    yl <- yh <- rnorm(10)

    expect_error(coefbounds(yl + yh ~ x1,
                            boot = 0,
                            cluster_id = seq_len(9)),
                 "9 observations")
    expect_error(coefbounds(yl + yh ~ x1,
                            boot = 0,
                            cluster_id = seq_len(11)),
                 "11 observations")
})

test_that("draw_boot_ind() produces expected output", {
    set.seed(210)
    cluster_id <- c(10, 10, 20, 20, 10)
    boot_ind <- replicate(100, draw_boot_ind(cluster_id))
    expect_true(all(boot_ind %in% list(c(1, 2, 5, 1, 2, 5),
                                       c(1, 2, 5, 3, 4),
                                       c(3, 4, 1, 2, 5),
                                       c(3, 4, 3, 4))))
})

test_that("print.interval_hypothesis() recognizes cluster bootstrap", {
    set.seed(7482)
    x1 <- rnorm(100)
    yl <- yh <- rnorm(100)
    fit_clus <- coefbounds(yl + yh ~ x1,
                           boot = 10,
                           cluster_id = sample(seq_along(x1), replace = TRUE))
    fit_no <- coefbounds(yl + yh ~ x1,
                         boot = 10)

    hyp_clus <- interval_hypothesis(fit_clus, "x1", c(0, 0))
    hyp_no <- interval_hypothesis(fit_no, "x1", c(0, 0))

    expect_output(print(hyp_clus),
                  "sqrt(Nclus)",
                  fixed = TRUE)
    expect_output(print(hyp_no),
                  "sqrt(N)",
                  fixed = TRUE)
})
