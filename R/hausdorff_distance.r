##' Calculates the directed or undirected Hausdorff distance between pairs of
##' intervals.
##' @title Hausdorff distance between intervals
##' @param x,y 2-column matrices, where the first column is pointwise less than
##' or equal to the second
##' @param directed logical: calculate the directed distance?
##' @return vector of distances
##' @author Brenton Kenkel
##' @keywords internal
hausdorff_distance <- function(x, y, directed = TRUE)
{
    ## x and y should be matrices
    if (is.vector(x))
        x <- t(as.matrix(x))
    if (is.vector(y))
        y <- t(as.matrix(y))

    ## Each entry should be the endpoints of an interval
    if (ncol(x) != 2 | ncol(y) != 2)
        stop("x and y should each have two columns")
    if (any(x[, 1] > x[, 2]) || any(y[, 1] > y[, 2]))
        stop("first endpoint cannot exceed the second")

    ## Calculate distance from each endpoint of x to the nearest point of y, and
    ## return whichever is greater
    dist_xy <- pmax(point_to_interval_distance(x[, 1], y),
                    point_to_interval_distance(x[, 2], y))

    ## If undirected requested: calculate distance from y to x and return the
    ## pointwise max
    if (!directed) {
        dist_yx <- Recall(x = y, y = x, directed = TRUE)
        dist_xy <- pmax(dist_xy, dist_yx)
    }

    dist_xy
}

##' Calculates the distance from each point to the closest element of the
##' corresponding interval.  Doesn't check validity of arguments; should only be
##' called within hausdorff_distance().
##' @title Distance from a point to an interval
##' @param point vector of points
##' @param interval matrix of intervals, with 2 columns and number of rows equal
##' to the length of \code{point}
##' @return vector of distances
##' @author Brenton Kenkel
##' @keywords internal
point_to_interval_distance <- function(point, interval)
{
    ifelse(interval[, 1] <= point & point <= interval[, 2],
           0,
           pmin(abs(point - interval[, 1]),
                abs(point - interval[, 2])))
}
