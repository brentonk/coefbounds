## Appease R CMD check
if (getRversion() >= "3.1.0")
    utils::globalVariables(c("i", "term"))

##' Bootstrap coefficient bounds
##' @param boot number of bootstrap iterations
##' @param cluster_id cluster membership vector
##' @param remove_collinear if \code{TRUE}, removes bootstrap iterations with a
##'     collinear design matrix (with warning); otherwise, any collinearity
##'     triggers a failure
##' @inheritParams coefbounds_fit
##' @return List of boot x 2 matrices of bootstrap bound estimates (one list
##'     element per term)
##' @author Brenton Kenkel
##' @keywords internal
##' @import foreach
boot_coefs <- function(boot, cluster_id, remove_collinear, YL, YU, X, Z, model, maxit)
{
    coefs <- foreach (i = iterators::icount(boot)) %do% {
        ## Draw bootstrap resample
        if (is.null(cluster_id)) {
            ind_boot <- sample(seq_len(nrow(X)),
                               size = nrow(X),
                               replace = TRUE)
        } else {
            ind_boot <- draw_boot_ind(cluster_id)
        }
        X_boot <- X[ind_boot, , drop = FALSE]
        Z_boot <- Z[ind_boot, , drop = FALSE]
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
            bd_boot <- coefbounds_fit(YL = YL_boot,
                                      YU = YU_boot,
                                      X = X_boot,
                                      Z = Z_boot,
                                      model = model,
                                      maxit = maxit)
        }

        bd_boot
    }

    ## Should return a list of estimated bounds by coefficient
    out_list <- foreach (term = seq_len(ncol(X))) %do% {
        term_rows <- lapply(coefs, function(x) x[term, ])
        do.call("rbind", term_rows)
    }
    names(out_list) <- colnames(X)

    out_list
}

##' Draw indices for cluster bootstrap
##' @inheritParams coefbounds
##' @return vector of indices
##' @author Brenton Kenkel
##' @keywords internal
draw_boot_ind <- function(cluster_id)
{
    unq_id <- unique(cluster_id)
    samp <- sample(unq_id,
                   size = length(unq_id),
                   replace = TRUE)
    ind <- lapply(samp, function(x) which(cluster_id == x))
    unlist(ind)
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
