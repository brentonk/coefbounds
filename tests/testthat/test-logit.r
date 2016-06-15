context("Logistic regression bounds")

test_that("coefbounds() enforces binary outcomes", {
    set.seed(471)
    x1 <- rnorm(10)
    yl <- rnorm(10)
    yu <- yl + 1

    expect_error(coefbounds(yl + yu ~ x1, model = "logit"),
                 "binary")
})

test_that("coefbounds() equals glm() when point-identified", {
    set.seed(72403)
    dat <- data.frame(yl = rbinom(100, 1, 0.5))
    dat$yu <- dat$yl
    for (i in 1:10)
        dat[[paste0("x", i)]] <- rnorm(100)

    fit_glm <- glm(yl ~ . - yu,
                   family = binomial(link = "logit"),
                   data = dat)
    fit_bd <- coefbounds(yl + yu ~ .,
                         data = dat,
                         model = "logit",
                         boot = 0)

    expect_equal(coef(fit_glm),
                 coef(fit_bd)[, 1])
    expect_equal(coef(fit_glm),
                 coef(fit_bd)[, 2])
})
