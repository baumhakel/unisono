# geoR is required to run
if (requireNamespace("geoR", quietly = TRUE)) {

  set.seed(1)

  # simple example ----------------------------------------------------------
  # 2D domain, univariate, anisotropic
  sim1 <- geoR::grf(
    grid = expand.grid(1:15, 1:15),
    cov.pars = c(1, 1),
    aniso.pars = c(0, 2)
  )
  res1 <- uni_iso_grid(sim1, window_dims = c(5, 5))
  # all proportions lie outside joint confidence intervals
  print(summary(res1))
  plot(res1)
}

\donttest{
  if (requireNamespace("geoR", quietly = TRUE)) {
    # custom lag set, multivariate --------------------------------------------
    sim2 <- geoR::grf(
      grid = expand.grid(1:15, 1:15),
      nsim = 2,
      cov.pars = c(1, 2),
      aniso.pars = c(0, 2)
    )

    # test 2 sets of lags simultaneously
    lags_list <- list(
      rbind(diag(2, 2), diag(-2, 2)),
      matrix(c(1, 1, -1, -1, 1, -1, 1, -1), nrow = 4, ncol = 2)
    )

    res2 <- uni_iso_grid(sim2, lags_list = lags_list, window_dims = c(5, 5))
    # anisotropy is not detected in all directions
    print(summary(res2))
    plot(res2)

    # + custom objective function (sum of absolute differences, vectorized)
    f2 <- function(z0, zlagged) {
      diffs <- zlagged -
        matrix(rep(z0, nrow(zlagged)), nrow = nrow(zlagged), byrow = TRUE)
      return(rowSums(abs(diffs)))
    }
    res2a <- uni_iso_grid(
      sim2,
      objfunc = f2,
      lags_list = lags_list,
      window_dims = c(5, 5)
    )


    # 3D domain, list-valued --------------------------------------------------

    # generate exemplary list-valued field
    sim3 <- geoR::grf(
      grid = expand.grid(1:7, 1:7, 1:7),
      nsim = 2,
      cov.pars = c(1, 0.25)
    )
    dat3 <- sim3$dat
    dat3 <- ifelse(dat3 > 0, "+", "-")
    sim3.l <- lapply(1:nrow(dat3), function(i) dat3[i, ])
    dim(sim3.l) <- c(7, 7, 7)

    # custom objective function (x is single observation, y is list of observations)
    f3 <- function(x, yl) {
      return(sapply(yl, function(y) {
        sum(unlist(x) != unlist(y))
      }))
    }

    res3 <- uni_iso_grid(sim3.l, objfunc = f3)
    print(summary(res3))
    plot(res3)
  }
}
