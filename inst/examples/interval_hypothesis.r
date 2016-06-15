## Simulate data
set.seed(18)
x1 <- rnorm(50)
x2 <- rnorm(50)
y <- 1 - x1 + x2 + rnorm(50)
yl <- floor(y)
yu <- ceiling(y)

## Fit model
fit <- coefbounds(yl + yu ~ x1 + x2, boot = 100)

## Test hypothesis that the identification region for the coefficient on x1
## contains -0.25
interval_hypothesis(fit,
                    term = "x1",
                    interval = c(-0.25, -0.25),
                    type = "subset")

## Test hypothesis that the identification region for the coefficient on x2
## equals [0, 1.5]
interval_hypothesis(fit,
                    term = "x2",
                    interval = c(0, 1.5),
                    type = "equal")
