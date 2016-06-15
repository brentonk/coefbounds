context("Linear model bounds")

test_that("coefbounds() fails on bad input", {
    set.seed(471)
    x1 <- rnorm(10)
    x2 <- rnorm(10)
    x3 <- x1
    yl <- rnorm(10)
    yu <- yl + 1

    expect_error(coefbounds(yl + yu ~ x1 + x3),
                 "collinear")
    expect_error(coefbounds(yl ~ x1 + x2),
                 "two terms")
    expect_error(coefbounds(yl + yu + x3 ~ x1 + x2),
                 "two terms")
    expect_error(coefbounds(yu + yl ~ x1 + x2),
                 "exceed upper bounds")
})

test_that("coefbounds() properly handles collinearity in bootstrap", {
    x1 <- c(1, 0, 0, 0, 0)
    yl <- rnorm(5)
    yu <- yl + 1

    set.seed(22)
    expect_warning(fit <- coefbounds(yl + yu ~ x1,
                                     boot = 10,
                                     remove_collinear = TRUE),
                   "collinear design matrix")
    expect_true(nrow(fit$dist[[1]]) < 10)

    set.seed(22)
    expect_error(coefbounds(yl + yu ~ x1,
                            boot = 10,
                            remove_collinear = FALSE),
                 "collinear design matrix")
})

test_that("coefbounds() without covariates returns means", {
    set.seed(200)
    yl <- rnorm(100)
    yu <- yl + exp(100)
    fit <- coefbounds(yl + yu ~ 1, boot = 0)

    expect_equal(coef(fit)["(Intercept)", "lower"],
                 mean(yl))
    expect_equal(coef(fit)["(Intercept)", "upper"],
                 mean(yu))
})

test_that("coefbounds() equals lm() when point-identified", {
    set.seed(8998)
    dat <- data.frame(yl = rnorm(100))
    dat$yu <- dat$yl
    for (i in 1:20)
        dat[[paste0("x", i)]] <- rnorm(100)

    fit_lm <- lm(yl ~ . - yu, data = dat)
    fit_bd <- coefbounds(yl + yu ~ .,
                         data = dat,
                         boot = 0)

    expect_equal(coef(fit_lm),
                 coef(fit_bd)[, 1])
    expect_equal(coef(fit_lm),
                 coef(fit_bd)[, 2])
})

test_that("coefbounds() computes distances correctly", {
    set.seed(14087)
    x1 <- rnorm(100)
    x2 <- rnorm(100)
    yl <- 1 + x1 - x2 + rnorm(100)
    yu <- yl + rexp(100)
    fit <- coefbounds(yl + yu ~ x1 + x2,
                      boot = 10,
                      return_boot_est = TRUE)

    ## Compare to (3.2) and (3.3) in Beresteanu and Molinari
    H <- t(fit$boot[[2]]) - coef(fit)[2, ]
    H <- abs(H)
    H <- pmax(H[1, ], H[2, ])
    H <- sqrt(length(x1)) * H
    expect_equal(H,
                 fit$dist[[2]][, "undirected"])

    dH <- t(fit$boot[[3]]) - coef(fit)[3, ]
    dH[2, ] <- dH[2, ] * -1
    dH[dH < 0] <- 0
    dH <- pmax(dH[1, ], dH[2, ])
    dH <- sqrt(length(x1)) * dH
    expect_equal(dH,
                 fit$dist[[3]][, "directed"])
})

test_that("coefbounds() returns same results as Stoye formula", {
    set.seed(76432)
    x1 <- rnorm(100)
    x2 <- rnorm(100)
    yl <- 1 + x1 - x2 + rnorm(100)
    yu <- yl + rexp(100)
    fit <- coefbounds(yl + yu ~ x1 + x2,
                      boot = 0)

    ## Stoye formula
    X <- cbind(1, x1, x2)
    mu <- X %*% solve(crossprod(X))
    for (d in 1:3) {
        y_lwr <- ifelse(mu[, d] > 0, yl, yu)
        y_upr <- ifelse(mu[, d] > 0, yu, yl)
        cf_lwr <- lsfit(x = X, y = y_lwr, intercept = FALSE)$coefficients[d]
        cf_upr <- lsfit(x = X, y = y_upr, intercept = FALSE)$coefficients[d]
        expect_equivalent(cf_lwr, coef(fit)[d, 1])
        expect_equivalent(cf_upr, coef(fit)[d, 2])
    }
})
