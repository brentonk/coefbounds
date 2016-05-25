##' Calculate critical values from bootstrap results
##' @param fit Object of class \code{"coefbounds"}
##' @param term Index or name of term in \code{fit}
##' @param level Confidence level
##' @param directed Whether to calculate the critical value for the directed or
##'     undirected Hausdorff distances
##' @return Critical value for the given level
##' @author Brenton Kenkel
##' @keywords internal
calculate_crit <- function(fit,
                           term,
                           level = 0.95,
                           directed = TRUE)
{
    if (is.null(fit$dist))
        stop("no bootstrap results available")

    type <- if (directed) "directed" else "undirected"

    crit <- stats::quantile(fit$dist[[term]][, type],
                            probs = level)
    unname(crit)
}
