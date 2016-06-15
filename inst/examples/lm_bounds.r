## Simulate data
set.seed(18)
x1 <- rnorm(50)
x2 <- rnorm(50)
y <- 1 - x1 + x2 + rnorm(50)
yl <- floor(y)
yu <- ceiling(y)

## Fit model without covariates
fit_mean <- coefbounds(yl + yu ~ 1, boot = 0)
all.equal(coef(fit_mean)[1, "lower"], mean(yl))
all.equal(coef(fit_mean)[1, "upper"], mean(yu))

## Fit model with covariates
fit_full <- coefbounds(yl + yu ~ x1 + x2, boot = 10)
coef(fit_full)
