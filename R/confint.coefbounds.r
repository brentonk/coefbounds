##' Bootstrap estimates of confidence regions for the bounds on model
##' coefficients.
##'
##' Two types of confidence region are available, both defined in Beresteanu and
##' Molinari (2008, Section 2).  The default is \code{"DU"}, the confidence
##' region based on the directed Hausdorff distance.  We would fail to reject
##' the null hypothesis that \eqn{B} is a subset of the population
##' identification region for the coefficient \eqn{\beta_j} if and only if
##' \eqn{B \subseteq DU(\beta_j)}.  The other option is \code{"CC"}, the
##' confidence collection based on the undirected Hausdorff distance.  We would
##' fail to reject the null hypothesis that \eqn{[b_L, b_U]} equals the
##' population identification region for \eqn{\beta_j} if and only if \eqn{b_L
##' \in CC_L(\beta_j)} and \eqn{b_U \in CC_U(\beta_j)}.
##'
##' The \code{"DU"} confidence region corresponds to
##' \code{\link{interval_hypothesis}} with \code{type = "subset"}.  The
##' \code{"CC"} confidence region corresponds to
##' \code{\link{interval_hypothesis}} with \code{type = "equal"}.
##'
##' Implements largely the same functionality as \code{CI1D} in Beresteanu et
##'     al.'s (2010) Stata program.
##' @title Confidence regions for coefficient bounds
##' @param object Fitted model of class \code{"coefbounds"}.  Must have been run
##'     with \code{boot > 0} and contain a non-null \code{dist} element.
##' @param type Whether to return the DU confidence set or the CC confidence
##'     collections.  See Details for explanation.
##' @param parm Names or indices of the coefficients for which to estimate
##'     confidence sets.  If not specified, uses all coefficients.
##' @param level Confidence level.
##' @param ... Not used.
##' @return A matrix containing the range of the confidence region for each term
##'     requested.  When \code{type== "CC"}, there are two entries per term
##'     (range of the confidence region for the lower and upper bound).
##' @author Brenton Kenkel
##' @references Arie Beresteanu and Francesca Molinari.  2008.
##'     "Asymptotic Properties for a Class of Partially Identified Models."
##'     \emph{Econometrica} 76 (4): 763--814.
##'
##' Arie Beresteanu, Francesca Molinari and Darcy Steeg Morris.  2010.
##'     "Asymptotics for Partially Identified Models in Stata."
##'     \url{https://molinari.economics.cornell.edu/programs.html}
##' @import foreach
##' @importFrom stats coef nobs
##' @export
##' @example inst/examples/confint.coefbounds.r
confint.coefbounds <- function(object,
                               parm = NULL,
                               level = 0.95,
                               type = c("DU", "CC"),
                               ...)
{
    type <- match.arg(type)
    directed <- type == "DU"

    ## Sanity checks
    cf_names <- rownames(coef(object))
    if (is.null(parm))
        parm <- cf_names
    if (is.numeric(parm))
        parm <- cf_names[parm]
    if (!all(parm %in% cf_names)) {
        stop("requested terms not in the model: ",
             paste(setdiff(parm, cf_names), collapse = ", "))
    }

    ## For each coefficient requested, retrieve the corresponding critical value
    ## and calculate the confidence interval
    ##
    ## Based on the formulas in Beresteanu and Molinari (2008, p. 781)
    cf <- coef(object)
    q <- (1 - level) / 2
    intervals <- foreach (term = parm, .combine = "rbind") %do% {
        crit <- calculate_crit(fit = object,
                               term = term,
                               level = level,
                               directed = directed)
        radius <- crit / sqrt(nobs(object, type = "cluster"))

        if (directed) {
            lwr <- cf[term, "lower"] - radius
            upr <- cf[term, "upper"] + radius
            row_names <- term
        } else {
            lwr <- cf[term, ] - radius
            upr <- cf[term, ] + radius
            row_names <- paste(term,
                               c("lower", "upper"),
                               sep = ".")
        }

        ans <- matrix(c(lwr, upr),
                      ncol = 2,
                      dimnames = list(
                          row_names,
                          format.perc(c(q, 1 - q), 3)
                      ))

        ans
    }

    intervals
}

##' stats:::format.perc
##' @param probs probabilities
##' @param digits number of digits
##' @return (kinda) pretty names, consistent with those used in the default
##'     confint() method
##' @keywords internal
format.perc <- function(probs, digits)
{
    paste(format(100 * probs,
                 trim = TRUE,
                 scientific = FALSE,
                 digits = digits),
          "%")
}
