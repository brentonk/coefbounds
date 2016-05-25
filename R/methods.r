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
