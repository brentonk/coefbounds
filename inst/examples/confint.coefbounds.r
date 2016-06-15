## Simulate data
set.seed(18)
x1 <- rnorm(50)
x2 <- rnorm(50)
y <- 1 - x1 + x2 + rnorm(50)
yl <- floor(y)
yu <- ceiling(y)

## Fit model
fit <- coefbounds(yl + yu ~ x1 + x2, boot = 100)

## Calculate DU confidence region for coefficient on x1
confint(fit, parm = "x1")

## Calculate CC confidence collections for all coefficients
confint(fit, type = "CC")
