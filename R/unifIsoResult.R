#' Constructor for unifIsoResult objects
#'
#' @description
#' Creates an object of class \code{unifIsoResult}, which stores the results of
#' the uniformity-based isotropy test for spatial random fields.
#'
#' @param statistic Numeric scalar. The overall test statistic.
#' @param stat_vec Numeric vector. The test statistics for each individual lag set.
#' @param p_value Numeric scalar. The computed p-value for the test.
#' @param moments List. Contains the estimated long-run covariance matrix and proportions.
#' @param eigenvalues Numeric vector. Eigenvalues of the estimated covariance matrix.
#' @param n_obs Numeric vector. The number of observations used for each lag set.
#' @param lags_list List of matrices. The lag vectors evaluated.
#' @param window_dims Integer vector. Dimensions of the spatial moving window.
#' @param overlap_dims Integer vector. Dimensions of overlap for the moving window.
#'
#' @return An object of class \code{unifIsoResult}.
#'
#' @name unifIsoResult
#' @keywords internal
new_unifIsoResult <- function(statistic, stat_vec, p_value, moments,
                              eigenvalues, n_obs, lags_list, window_dims, overlap_dims) {
  structure(
    list(
      statistic   = statistic,
      stat_vec    = stat_vec,
      p_value     = p_value,
      moments     = moments,
      eigenvalues = eigenvalues,
      n_obs       = n_obs,
      lags_list   = lags_list,
      window_dims = window_dims,
      overlap_dims = overlap_dims
    ),
    class = "unifIsoResult"
  )
}

#' Print method for unifIsoResult objects
#'
#' @param x An object of class \code{unifIsoResult}.
#' @param digits Integer. Number of significant digits to print.
#' @param ... Additional arguments passed to \code{print}.
#'
#' @return Invisibly returns the original object \code{x}.
#' @export
#'
#' @examples
#' \dontrun{
#' # TODO: Add basic printing example here
#' }
print.unifIsoResult <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("Isotropy test for random fields on a grid\n")
  cat("\n")
  cat("Test statistic:    ", formatC(x$statistic, digits = digits, format = "g"), "\n")
  cat("p-value:           ", format.pval(x$p_value, digits = digits), "\n")
  cat("Number of lag sets:", length(x$lags_list), "\n")
  cat("Number of lags:    ", sum(vapply(x$lags_list, nrow, 1)), "\n")
  invisible(x)
}


#' Summary method for unifIsoResult objects
#'
#' @param object An object of class \code{unifIsoResult}.
#' @param alpha Numeric scalar. The significance level for confidence intervals.
#'   Default is 0.05.
#' @param ... Additional arguments passed to \code{summary}.
#'
#' @return An object of class \code{summary.unifIsoResult}.
#' @export
#'
#' @examples
#' \dontrun{
#' # TODO: Add basic summary example here
#' }
summary.unifIsoResult <- function(object, alpha = 0.05, ...) {

  # Compute confidence intervals and format the output table
  lag_table <- compute_ci_table(object, alpha = alpha) |>
    dplyr::select("set", Lag = "label", "length", "prop", "ci_lower", "ci_upper") |>
    dplyr::rename(Family = "set", Proportion = "prop", CI_lower = "ci_lower", CI_upper = "ci_upper") |>
    dplyr::mutate(Family = as.factor(.data$Family),
                  Proportion = round(.data$Proportion, 3),
                  CI_lower = round(.data$CI_lower, 3),
                  CI_upper = round(.data$CI_upper, 3))


  # Split the formatted table into a list of data frames (one per lag family)
  # Append the expected proportion under the null hypothesis of isotropy
  lag_table <- split(lag_table, lag_table$Family) |>
    lapply(function(df) {
      df$Expected <- round(1 / nrow(df),3)
      df
    })

  structure(
    list(
      statistic      = object$statistic,
      p_value        = object$p_value,
      alpha          = alpha,
      reject         = object$p_value < alpha,
      n_lag_families = length(object$lags_list),
      lag_table      = new_unifIsoResultLagTable(lag_table),
      n_windows      = object$moments$n_windows,
      window_dims    = object$window_dims,
      overlap_dims   = object$overlap_dims
    ),
    class = "summary.unifIsoResult"
  )
}

#' Print method for summary.unifIsoResult objects
#'
#' @param x An object of class \code{summary.unifIsoResult}.
#' @param digits Integer. Number of significant digits to print.
#' @param ... Additional arguments passed to \code{print}.
#'
#' @return Invisibly returns the original object \code{x}.
#' @export
#'
#' @examples
#' \dontrun{
#' # TODO: Add basic print.summary example here
#' }
print.summary.unifIsoResult <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  # Print header, test statistics, and test decision
  cat("Isotropy test for random fields on a grid\n")
  cat(strrep("-", 40), "\n")
  cat("Test statistic:", formatC(x$statistic, digits = digits, format = "g"), "\n")
  cat("p-value:       ", format.pval(x$p_value, digits = digits), "\n")
  cat("Decision at alpha =", x$alpha, ":",
      if (x$reject) "reject isotropy" else "fail to reject isotropy", "\n")
  cat("\n")

  # Print details about spatial subregions and grid configuration
  cat("Subsampling setup:\n")
  cat("  Number of subregions:", x$n_windows, "\n")
  if (!is.null(x$window_dims)) {
    cat("  Subregion dimensions:", paste(x$window_dims, collapse = " x "), "\n")
  }
  if (!is.null(x$overlap_dims)) {
    cat("  Overlap dimensions:  ", paste(x$overlap_dims, collapse = " x "), "\n")
  }
  cat("\n")

  # Output table of tested lag families and empirical proportions
  cat("Lag families tested (", x$n_lag_families, " total):\n", sep = "")

  print(x$lag_table)

  invisible(x)
}


#' Plot method for unifIsoResult objects
#'
#' @description
#' Generates diagnostic plots for spatial isotropy tests. Automatically selects
#' between a rose plot (for 2D spatial lags) and a lollipop plot.
#'
#' @param x An object of class \code{unifIsoResult}.
#' @param alpha Numeric scalar. Significance level for plotted intervals. Default is 0.05.
#' @param force_lollipop Logical. If \code{TRUE}, forces rendering of a lollipop plot
#'   even for 2D fields. Default is \code{FALSE}.
#' @param drawlabs Logical. If \code{TRUE}, draws axis/plot labels. Default is \code{TRUE}.
#' @param cols Optional vector of colors for the plot.
#' @param setnames Logical. If \code{TRUE}, uses set names in the legend/labels. Default is \code{TRUE}.
#' @param ... Additional arguments passed to the underlying plotting functions.
#'
#' @return A plot object (depends on underlying functions, typically \code{ggplot}).
#' @export
#'
#' @examples
#' \dontrun{
#' # TODO: Add plotting example here
#' }
plot.unifIsoResult <- function(x, alpha = 0.05, force_lollipop = FALSE, drawlabs = TRUE,
                               cols = NULL, setnames = TRUE, ...) {
  # Select rose plot for 2-dimensional lags unless explicitly overridden
  if (ncol(x$lags_list[[1]]) == 2 && !force_lollipop) {
    unifIsoResult_roseplot(x, alpha = alpha, drawlabs = drawlabs,
                           cols = cols, setnames = setnames, ...)
  } else {
    unifIsoResult_lollipop(x, alpha = alpha, drawlabs = drawlabs,
                           cols = cols, setnames = setnames, ...)
  }

}


#' Compute Confidence Interval Table for Isotropy Results
#'
#' @description
#' Calculates multivariate normal confidence intervals around the empirical
#' lag proportions based on the estimated asymptotic covariance matrix.
#'
#' @param uni.res An object of class \code{unifIsoResult}.
#' @param alpha Numeric scalar. The significance level for the confidence intervals.
#'
#' @return A \code{tibble} containing the lags, estimated proportions, and confidence bounds.
#'
#' @keywords internal
#' @noRd
compute_ci_table <- function(uni.res, alpha = 0.05) {
  # Validate input object and significance level
  if (!inherits(uni.res, "unifIsoResult")) {
    stop("Input must be of class 'unifIsoResult'.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single numeric value between 0 and 1.")
  }

  nlags.v <- vapply(uni.res$lags_list, nrow, 1)
  nlags.total <- sum(nlags.v)

  # Ensure the covariance matrix is strictly positive definite by adding a small nugget
  # based on the minimum eigenvalue
  Sigma <- uni.res$moments$sig + diag(min(eigen(uni.res$moments$sig)$values) + 1e-10, nlags.total)

  corm <- stats::cov2cor(Sigma)
  corm <- (corm + t(corm)) / 2 # Symmetrize the correlation matrix to correct floating point inaccuracies

  se <- sqrt(diag(Sigma))
  c_alpha <- mvtnorm::qmvnorm(1 - alpha, mean = 0, corr = corm)$quantile
  intervals <- cbind(uni.res$stat_vec - c_alpha * se, uni.res$stat_vec + c_alpha * se)

  nobs.v.reps <- rep(vapply(uni.res$n_obs, identity, 1), nlags.v)
  mean.v.reps <- rep(1 / nlags.v, nlags.v)

  # Rescale normal intervals back to proportional space
  intervals.mean <- intervals / sqrt(nobs.v.reps) + mean.v.reps
  meanvec <- uni.res$stat_vec / sqrt(nobs.v.reps) + mean.v.reps

  lags_list.mat <- do.call(rbind, uni.res$lags_list)

  # Generate string identifiers for lag sets and individual lag vectors
  classvec <- unlist(lapply(seq_along(uni.res$lags_list), function(i) {
    rep(paste0("set ", i), nlags.v[i])
  }))
  label_vec <- apply(lags_list.mat, 1, function(r) {
    paste0("(", paste(round(r, 1), collapse = ","), ")")
  })

  tibble::tibble(
    set      = classvec,
    lags     = lags_list.mat,
    prop     = meanvec,
    ci_lower = intervals.mean[, 1],
    ci_upper = intervals.mean[, 2],
    label    = label_vec
  ) |>
    dplyr::mutate(length = sqrt(rowSums(.data$lags^2)))
}
