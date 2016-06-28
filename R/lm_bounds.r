##' Bounds for all coefficients in a linear model
##' @inheritParams coefbounds_fit
##' @param ... for safety
##' @return matrix containing the estimated bounds
##' @author Brenton Kenkel
##' @keywords internal
lm_bounds <- function(YL, YU, X, Z, ...)
{
    fit_d <- function(d, YL, YU, X.)
    {
        ## From Corollary 4.5 of B&M
        xd <- X.[, d]
        if (ncol(X.) > 1) {
            xd <- stats::lsfit(x = X.[, -d], y = xd, intercept = FALSE)$residuals
        }

        lb <- sum(pmin(xd * YL, xd * YU))
        ub <- sum(pmax(xd * YL, xd * YU))

        c(lb, ub) / sum(xd^2)
    }

    ## Calculate design matrix for 2SLS if requested
    ##
    ## Code from AER::ivreg.fit
    if (!is.null(Z)) {
        auxreg <- stats::lm.fit(Z, X)
        XZ <- as.matrix(auxreg$fitted.values)
        colnames(XZ) <- colnames(X)
        X <- XZ
    }

    bounds <- sapply(seq_len(ncol(X)),
                     fit_d,
                     YL = YL,
                     YU = YU,
                     X. = X)

    bounds <- t(bounds)
    rownames(bounds) <- colnames(X)
    colnames(bounds) <- c("lower", "upper")

    bounds
}
