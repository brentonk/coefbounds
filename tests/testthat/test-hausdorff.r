context("Hausdorff distance")

test_that("hausdorff_distance() fails on bad input", {
    expect_error(hausdorff_distance(1:3, 1:2),
                 "two columns")
    expect_error(hausdorff_distance(matrix(1:6, ncol = 3), 1:2),
                 "two columns")
    expect_error(hausdorff_distance(2:1, 1:2),
                 "cannot exceed")
})

test_that("hausdorff_distance() produces correct results", {
    x <- rbind(c(-1, 1),
               c(1, 1),
               c(2, 3),
               c(-3, 4))
    y <- rbind(c(-1, 1),
               c(-0.25, 2),
               c(1, 2.5),
               c(-2, 0.5))

    d_xy <- hausdorff_distance(x, y, directed = TRUE)
    d_yx <- hausdorff_distance(y, x, directed = TRUE)
    dH_xy <- hausdorff_distance(x, y, directed = FALSE)
    dH_yx <- hausdorff_distance(y, x, directed = FALSE)

    expect_equal(d_xy, c(0, 0, 0.5, 3.5))
    expect_equal(d_yx, c(0, 1.25, 1, 0))

    expect_equal(dH_xy, pmax(d_xy, d_yx))
    expect_equal(dH_xy, dH_yx)

    expect_equal(d_xy[3],
                 hausdorff_distance(x[3, ], y[3, ]))
    expect_equal(d_xy[4],
                 hausdorff_distance(x, y[4, ])[4])
})
