## Appease R CMD check
if (getRversion() >= "3.1.0")
    utils::globalVariables(c("i", "term"))

##' Estimates the projections of the identification region along each
##' coefficient dimension for a linear model with interval-censored outcomes
##' (Beresteanu and Molinari 2008, Corollary 4.5).  If requested, uses a
##' nonparametric bootstrap to estimate critical values for hypothesis tests
##' about these projections (Beresteanu and Molinari 2008, Algorithm 4.2).
##'
##' Implements largely the same functionality as \code{oneDproj} and
##'     \code{CI1D} in Beresteanu et al.'s (2010) Stata program.
##' @title Coefficient bounds for linear models
##' @param formula Model formula of the form \code{yl + yu ~ x1 + x2 + ...},
##'     where \code{yl} is the lower bound on the response, \code{yu} is the
##'     upper bound, and \code{x1 + x2 + ...} are the covariates.
##' @param data,subset,na.action As in \code{\link{lm}}
##' @param boot Number of bootstrap iterations used to estimate the critical
##'     values for inference.
##' @param remove_collinear How to treat boostrap iterations in which the design
##'     matrix is rank-deficient.  If \code{TRUE} (the default), a warning is
##'     issued and the bad iterations are removed.  If \code{FALSE}, the
##'     function fails with an error when a rank-deficient design matrix is
##'     encountered.
##' @param return_boot_est Whether to include the bootstrap estimates of the
##'     coefficient bounds in the returned object.
##' @return A list of class \code{"coefbounds"} containing:
##' \describe{
##' \item{\code{coefficients}}{Matrix containing the sample estimates of the
##'     coefficient bounds.}
##' \item{\code{dist}}{List of matrices containing the bootstrap Hausdorff
##'     distances (undirected and directed) used for inference.}
##' \item{\code{boot_est}}{(if requested) List of matrices of bootstrap
##'     estimates of the coefficient bounds.}
##' \item{\code{nobs}}{Number of observations used in fitting.}
##' \item{\code{call}}{Original function call.}
##' }
##' @author Brenton Kenkel
##' @references Arie Beresteanu and Francesca Molinari.  2008.
##'     "Asymptotic Properties for a Class of Partially Identified Models."
##'     \emph{Econometrica} 76 (4): 763--814.
##'
##' Arie Beresteanu, Francesca Molinari and Darcy Steeg Morris.  2010.
##'     "Asymptotics for Partially Identified Models in Stata."
##'     \url{https://molinari.economics.cornell.edu/programs.html}
##' @export
##' @example inst/examples/lm_bounds.r
lm_bounds <- function(formula,
                      data,
                      subset,
                      na.action,
                      boot = 100,
                      remove_collinear = TRUE,
                      return_boot_est = FALSE)
{
    cl <- match.call()

    ## Construct model frame the usual way
    Formula <- Formula::as.Formula(formula)
    mf <- match(c("data", "subset", "na.action"), names(cl), 0L)
    mf <- cl[c(1L, mf)]
    mf$formula <- Formula
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())

    ## Extract design matrix
    X <- stats::model.matrix(Formula, data = mf)
    if (qr(X)$rank < ncol(X))
        stop("design matrix is collinear")

    ## Extract response bounds and sanity check
    Y <- Formula::model.part(Formula, data = mf, lhs = 1)
    if (ncol(Y) != 2)
        stop("formula LHS must contain exactly two terms")
    YL <- Y[, 1]
    YU <- Y[, 2]
    if (any(YL > YU)) {
        bad_obs <- which(YL > YU)
        stop("lower bounds exceed upper bounds in observation",
             if (length(bad_obs) > 1) "s" else "",
             ": ",
             paste(rownames(mf)[bad_obs], collapse = ", "))
    }

    ## Calculate unidimensional coefficient bounds
    bd <- all_bounds(YL = YL,
                     YU = YU,
                     X. = X)

    ## Nonparametric bootstrap of bound estimates
    if (boot > 0) {
        bd_boot <- boot_coefs(boot = boot,
                              remove_collinear = remove_collinear,
                              YL = YL,
                              YU = YU,
                              X. = X)
        dist_boot <- boot_dist(fit_main = bd,
                               fit_boot = bd_boot,
                               n_obs = nrow(X))
    } else {
        bd_boot <- dist_boot <- NULL
    }

    structure(list(coefficients = bd,
                   dist = dist_boot,
                   boot_est = if (return_boot_est) bd_boot else NULL,
                   nobs = nrow(X),
                   call = cl),
              class = "coefbounds")
}

##' Bounds for a single coefficient
##' @param d column of design matrix to estimate bounds for
##' @param YL response lower bound
##' @param YU response upper bound
##' @param X. design matrix
##' @return numeric vector containing the estimated bounds
##' @author Brenton Kenkel
##' @keywords internal
coef_bounds <- function(d, YL, YU, X.)
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

##' Bounds for all coefficients
##' @inheritParams coef_bounds
##' @return matrix containing the estimated bounds
##' @author Brenton Kenkel
##' @keywords internal
all_bounds <- function(YL, YU, X.)
{
    bounds <- sapply(seq_len(ncol(X.)),
                     coef_bounds,
                     YL = YL,
                     YU = YU,
                     X. = X.)

    bounds <- t(bounds)
    rownames(bounds) <- colnames(X.)
    colnames(bounds) <- c("lower", "upper")

    bounds
}

##' Bootstrap coefficient bounds
##' @param boot number of bootstrap iterations
##' @param remove_collinear if \code{TRUE}, removes bootstrap iterations with a
##'     collinear design matrix (with warning); otherwise, any collinearity
##'     triggers a failure
##' @inheritParams coef_bounds
##' @return List of boot x 2 matrices of bootstrap bound estimates (one list
##'     element per term)
##' @author Brenton Kenkel
##' @keywords internal
##' @import foreach
boot_coefs <- function(boot, remove_collinear, YL, YU, X.)
{
    coefs <- foreach (i = iterators::icount(boot)) %do% {
        ## Draw bootstrap resample
        ind_boot <- sample(seq_len(nrow(X.)),
                           size = nrow(X.),
                           replace = TRUE)
        X_boot <- X.[ind_boot, , drop = FALSE]
        YL_boot <- YL[ind_boot]
        YU_boot <- YU[ind_boot]

        ## Check design matrix for collinearity before proceeding
        if (qr(X_boot)$rank < ncol(X_boot)) {
            if (!remove_collinear) {
                stop("collinear design matrix in bootstrap iteration ", i)
            } else {
                warning("collinear design matrix in bootstrap iteration ", i)
            }
            bd_boot <- NULL
        } else {
            bd_boot <- all_bounds(YL = YL_boot,
                                  YU = YU_boot,
                                  X. = X_boot)
        }

        bd_boot
    }

    ## Should return a list of estimated bounds by coefficient
    out_list <- foreach (term = seq_len(ncol(X.))) %do% {
        term_rows <- lapply(coefs, function(x) x[term, ])
        do.call("rbind", term_rows)
    }
    names(out_list) <- colnames(X.)

    out_list
}

##' Calculate bootstrap distances
##' @param fit_main Matrix of sample bound estimates
##' @param fit_boot List of matrices of bootstrap bound estimates, as returned
##'     by boot_coefs()
##' @param n_obs Number of observations
##' @return List of boot x 2 matrices of bootstrap Hausdorff distances
##'     multiplied by root-N (one list element per term, containing directed and
##'     undirected distances)
##' @author Brenton Kenkel
##' @keywords internal
##' @import foreach
boot_dist <- function(fit_main, fit_boot, n_obs)
{
    dist_vals <- foreach (term = seq_len(nrow(fit_main))) %do% {
        ans <- cbind(
            undirected = hausdorff_distance(x = fit_main[term, ],
                                            y = fit_boot[[term]],
                                            directed = FALSE),
            directed = hausdorff_distance(x = fit_main[term, ],
                                          y = fit_boot[[term]],
                                          directed = TRUE)
        )

        ans * sqrt(n_obs)
    }
    names(dist_vals) <- names(fit_boot)

    dist_vals
}
