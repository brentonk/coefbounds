## Appease R CMD check
if (getRversion() >= "3.1.0")
    utils::globalVariables("j")

##' Logit coefficients and variance matrix
##' @param Y response
##' @param X design matrix
##' @return list containing the coefficients and variance matrix
##' @author Brenton Kenkel
##' @keywords internal
logit_coefs <- function(Y, X)
{
    fit <- stats::glm.fit(x = X,
                          y = Y,
                          family = stats::binomial(link = "logit"))
    cf <- fit$coefficients

    ## Retrieve the variance-covariance matrix using the same method as in
    ## vcov.glm(), which calls summary.glm() and returns covmat.scaled, which
    ## for logits is the same as covmat.unscaled
    p <- fit$rank
    p1 <- 1L:p
    Qr <- fit$qr
    coef.p <- cf[Qr$pivot[p1]]
    covmat.unscaled <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
    dimnames(covmat.unscaled) <- list(names(coef.p), names(coef.p))
    vcv <- covmat.unscaled

    list(coef = cf, vcov = vcv)
}

##' Bounds for all coefficients in a logit model
##' @inheritParams coefbounds_fit
##' @param ... for safety
##' @return matrix containing the estimated bounds
##' @author Brenton Kenkel
##' @keywords internal
##' @import foreach
logit_bounds <- function(YL, YU, X, maxit, ...)
{
    ## Update step for inside the while loops
    update_bd <- function(j, bd, mu, lower)
    {
        ## Extremize the response
        if (lower) {
            y_update <- ifelse(mu > 0, YL, YU)
        } else {
            y_update <- ifelse(mu > 0, YU, YL)
        }

        ## Estimate model and variance matrix
        fit <- logit_coefs(Y = y_update,
                           X = X)

        ## Calculate d_beta / d_Y
        mu_new <- (X %*% fit$vcov)[, j]

        ## If all the signs of the new mu are the same as the old one, then the
        ## next estimate will be the same, so there's no point continuing
        brk <- all(sign(mu) == sign(mu_new))

        list(bd = if (lower) min(bd, fit$coef[j]) else max(bd, fit$coef[j]),
             mu = mu_new,
             brk = brk)
    }

    ## While loop for estimating each individual bound
    loop_bd <- function(j, bd_start, mu_start, lower, maxit)
    {
        bd <- bd_start
        mu <- mu_start
        iter <- 0
        while (iter < maxit) {
            upd <- update_bd(j = j,
                             bd = bd,
                             mu = mu,
                             lower = lower)
            bd <- upd$bd
            mu <- upd$mu
            iter <- iter + 1
            if (upd$brk)
                break
        }

        bd
    }

    ## Matrix whose entry signs determine extremizing responses exactly in the
    ## linear model -- used to initialize the while loops
    mu_lm <- X %*% solve(crossprod(X))

    ## Loop through each coefficient
    ans <- foreach (j = seq_len(ncol(X)), .combine = "rbind") %do% {
        bd_lwr <- loop_bd(j = j,
                          bd_start = Inf,
                          mu_start = mu_lm[, j],
                          lower = TRUE,
                          maxit = maxit)
        bd_upr <- loop_bd(j = j,
                          bd_start = -Inf,
                          mu_start = mu_lm[, j],
                          lower = FALSE,
                          maxit = maxit)

        c(bd_lwr, bd_upr)
    }
    rownames(ans) <- colnames(X)
    colnames(ans) <- c("lower", "upper")

    ans
}
