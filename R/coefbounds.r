##' Estimates the projections of the identification region along each
##' coefficient dimension for a linear or logistic regression model with
##' interval-censored outcomes (Beresteanu and Molinari 2008, Corollary 4.5).
##' If requested, uses a nonparametric bootstrap to estimate critical values for
##' hypothesis tests about these projections (Beresteanu and Molinari 2008,
##' Algorithm 4.2).
##'
##' In the linear case, implements largely the same functionality as
##'     \code{oneDproj} and \code{CI1D} in Beresteanu et al.'s (2010) Stata
##'     program.
##' @title Coefficient bounds for linear models
##' @param formula Model formula of the form \code{yl + yu ~ x1 + x2 + ...},
##'     where \code{yl} is the lower bound on the response, \code{yu} is the
##'     upper bound, and \code{x1 + x2 + ...} are the covariates.
##' @param data,subset,na.action As in \code{\link{lm}}
##' @param model \code{"linear"} for linear regression (default), \code{"logit"}
##'     for logistic regression.
##' @param boot Number of bootstrap iterations used to estimate the critical
##'     values for inference.
##' @param maxit Maximum number of iterations for the approximation in logistic
##'     regression models.  Ignored when \code{model = "linear"}.
##' @param remove_collinear How to treat boostrap iterations in which the design
##'     matrix is rank-deficient.  If \code{TRUE} (the default), a warning is
##'     issued and the bad iterations are removed.  If \code{FALSE}, the
##'     function fails with an error when a rank-deficient design matrix is
##'     encountered.
##' @param return_boot_est Whether to include the bootstrap estimates of the
##'     coefficient bounds in the returned object.
##' @return A list of class \code{"coefbounds"} containing: \describe{
##' \item{\code{coefficients}}{Matrix containing the sample estimates of the
##'     coefficient bounds.}
##' \item{\code{dist}}{List of matrices containing the
##'     bootstrap Hausdorff distances (undirected and directed) used for
##'     inference.}
##' \item{\code{boot_est}}{(if requested) List of matrices of
##'     bootstrap estimates of the coefficient bounds.}
##' \item{\code{nobs}}{Number of observations used in fitting.}
##' \item{\code{call}}{Original function call.}
##' \item{\code{model}}{Model used.}
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
coefbounds <- function(formula,
                       data,
                       subset,
                       na.action,
                       model = c("linear", "logit"),
                       boot = 100,
                       maxit = 10,
                       remove_collinear = TRUE,
                       return_boot_est = FALSE)
{
    cl <- match.call()
    model <- match.arg(model)

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
    if (model == "logit" && !all(c(YL, YU) %in% 0:1))
        stop("YL and YU must be binary in logit models")

    ## Calculate unidimensional coefficient bounds
    bd <- coefbounds_fit(YL = YL,
                         YU = YU,
                         X = X,
                         model = model,
                         maxit = maxit)

    ## Nonparametric bootstrap of bound estimates
    if (boot > 0) {
        bd_boot <- boot_coefs(boot = boot,
                              remove_collinear = remove_collinear,
                              YL = YL,
                              YU = YU,
                              X = X,
                              model = model,
                              maxit = maxit)
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
                   call = cl,
                   model = model),
              class = "coefbounds")
}

##' Direct to appropriate fitting function
##' @param YL response lower bound
##' @param YU response upper bound
##' @param X design matrix
##' @param model "linear" or "logit"
##' @param maxit max iterations for logit approximation
##' @return matrix containing the estimated bounds
##' @author Brenton Kenkel
##' @keywords internal
coefbounds_fit <- function(YL, YU, X, model, maxit)
{
    fn_fit <- switch(model,
                     logit = logit_bounds,
                     linear = lm_bounds)
    fn_fit(YL = YL,
           YU = YU,
           X = X,
           maxit = maxit)
}

##' @export
print.coefbounds <- function(x, ...)
{
    cat("\nCall:\n")
    print(x$call)

    cat("\nEstimated coefficient bounds:\n")
    stats::printCoefmat(x$coefficients)

    cat("\n")

    invisible(x)
}
