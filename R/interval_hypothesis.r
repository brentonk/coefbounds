##' Given an interval \eqn{B = [b_L, b_U]}, tests against the null hypothesis that
##' the identification region for the specified coefficient is a superset of
##' \eqn{B} (\code{type = "subset"}, the default) or equals \eqn{B} (\code{type
##' = "equal"}).
##'
##' The test with \code{type = "subset"} corresponds to
##' \code{\link{confint.coefbounds}} with \code{type = "DU"}.  The test with
##' \code{type = "equal"} corresponds to \code{\link{confint.coefbounds}} with
##' \code{type = "CC"}.
##'
##' Implements largely the same functionality as \code{CI1D} in Beresteanu et
##'     al.'s (2010) Stata program.
##' @title Hypothesis tests for identification regions
##' @param fit Fitted model of class \code{"coefbounds"}.  Must have been run
##'     with \code{boot > 0} and contain a non-null \code{dist} element.
##' @param term Name of the coefficient whose identification region is being
##'     tested.
##' @param interval Interval specified in the null hypothesis.  May be a
##'     singleton; e.g., \code{interval = c(0, 0)} to test that the
##'     identification region contains 0.
##' @param type Whether the null hypothesis should be that \code{interval} is a
##'     \code{"subset"} of the population identification region (the default) or
##'     is \code{"equal"} to it.
##' @return A list of class \code{"interval_hypothesis"} containing:
##' \describe{
##' \item{\code{test_stat}}{Test statistic.}
##' \item{\code{p}}{Bootstrap p-value.}
##' \item{\code{n_boot}}{Number of bootstrap iterations.}
##' \item{\code{term}}{Name of coefficient tested.}
##' \item{\code{interval}}{Interval specified.}
##' \item{\code{estimate}}{Sample estimate of coefficient bounds.}
##' \item{\code{type}}{Type of test performed.}
##' }
##' @author Brenton Kenkel
##' @references
##' Arie Beresteanu, Francesca Molinari and Darcy Steeg Morris.  2010.
##'     "Asymptotics for Partially Identified Models in Stata."
##'     \url{https://molinari.economics.cornell.edu/programs.html}
##' @importFrom stats coef
##' @export
##' @example inst/examples/interval_hypothesis.r
interval_hypothesis <- function(fit,
                                term,
                                interval,
                                type = c("subset", "equal"))
{
    type <- match.arg(type)
    directed <- type == "subset"

    if (is.null(fit$dist))
        stop("no bootstrap results available")
    if (length(term) > 1)
        stop("cannot test multiple terms")
    if (!(term %in% rownames(coef(fit))))
        stop("term ", term, " is not in the model")
    if (length(interval) != 2)
        stop("interval must be a numeric vector of length 2")
    if (interval[1] > interval[2])
        stop("lower bound of interval exceeds upper bound")

    ## Calculate specified Hausdorff distance and bootstrap p-value
    dist_boot <- fit$dist[[term]][, if (directed) "directed" else "undirected"]
    test_stat <- hausdorff_distance(x = interval,
                                    y = coef(fit)[term, ],
                                    directed = directed)
    test_stat <- test_stat * sqrt(stats::nobs(fit))
    p <- mean(test_stat <= dist_boot)

    structure(list(test_stat = test_stat,
                   p = p,
                   n_boot = nrow(fit$dist[[term]]),
                   term = term,
                   interval = interval,
                   estimate = coef(fit)[term, ],
                   type = type),
              class = "interval_hypothesis")
}

##' @export
print.interval_hypothesis <- function(x,
                                      digits = 3,
                                      ...)
{
    directed <- x$type == "subset"
    
    fmt_interval <- function(interval)
    {
        paste0("[",
               paste(format(interval, digits = digits),
                     collapse = ", "),
               "]")
    }
    fmt_hyp <- fmt_interval(x$interval)
    fmt_est <- fmt_interval(x$estimate)

    if (x$p < 1 / x$n_boot) {
        fmt_p <- paste0("<",
                        format(1 / x$n_boot, digits = digits))
    } else {
        fmt_p <- format(x$p, digits = digits)
    }

    output <- c(
        "",
        paste0("Null hypothesis: identification region for ",
               x$term,
               if (directed) " contains " else " equals ",
               fmt_hyp),
        paste0("Estimated identification region: ",
               fmt_est),
        paste0("Test statistic: sqrt(N) * ",
               if (directed) "directed ",
               "Hausdorff distance"),
        "",
        paste0("stat = ",
               format(x$test_stat, digits = digits),
               ", p-value = ",
               fmt_p),
        ""
    )

    writeLines(output)

    invisible(x)
}
