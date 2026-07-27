#' Find index of lag vector that maximizes objective function at each grid point
#'
#' @description
#' Evaluates a vectorized objective function over a set of directional lags for each point
#' in a spatial grid. Returns the index of the lag vector that maximizes (or minimizes)
#' the objective function. Dispatches to C++ routines for numeric arrays or a dedicated
#' list-handler for list-arrays.
#'
#' @param spdata A spatial data object. Can be a d-dimensional array (first d-1 dimensions
#'   are the spatial grid, third dimension is the coordinate index of a multivariate random field)
#'   or a list-array for complex objects.
#' @param objfunc The objective function to be evaluated over directions to obtain the argmax.
#'   Must be vectorized. For numeric data, this can also be the string \code{"normdiff"}.
#' @param lags Matrix of lags (each row is a lag vector, in scale of delta).
#' @param minimize Logical. Should the objective function be minimized instead of maximized?
#'   Default is \code{FALSE}.
#'
#' @return A matrix with the same spatial dimensions as \code{spdata} (first two dimensions),
#'   containing the index of the lag vector that optimizes the objective function at each grid point.
#'   Returns \code{NA} if not computable (e.g., near the boundary).
#'
#' @keywords internal
#' @noRd
get_argmax_indices <- function(spdata, objfunc, lags, minimize = F) {
  dtype <- class(spdata[1])

  # Dispatch based on the underlying data type of the spatial field
  if (dtype == "list") {
    res <- get_argmax_indices.list(spdata, objfunc, lags, minimize)
  } else if (dtype == "numeric") {
    if (is.function(objfunc)) {
      res <- argmaxLagIndexCpp(spdata, objfunc, lags, minimize)
    } else {
      # Use optimized C++ routine for standard squared norm differences
      if (objfunc == "normdiff") {
        res <- argmaxLagIndexSqDiffCpp(spdata, lags, minimize)
      } else {
        stop("Unsupported objfunc for numeric spdata. Must be a function or 'normdiff'.")
      }
    }
  }

  return(res)
}


#' Find index of lag vector that maximizes objective function at each grid point for list data
#'
#' @description
#' Resolves the optimizing lag index across a $d$-dimensional grid where each grid
#' point contains an arbitrary object (stored as a list). It uses a linear-offset
#' stride trick to efficiently compute lagged differences without nested loops.
#'
#' @param spdata A list-array with \code{dim(spdata)} of length $d$, containing one
#'   arbitrary object per grid point.
#' @param objfunc A function \code{objfunc(z0, zlagged)} where \code{zlagged} is a list
#'   of length \code{nh}.
#' @param lags An \code{nh x d} matrix of lags (assumed integer-valued).
#' @param minimize Logical. Should the objective function be minimized? Default is \code{FALSE}.
#'
#' @return An array with \code{dim(spdata)} holding the optimizing lag indices.
#'   Contains \code{NA} near the boundaries.
#'
#' @keywords internal
#' @noRd
get_argmax_indices.list <- function(spdata, objfunc, lags, minimize = F) {
  nvec <- dim(spdata)
  stopifnot(is.list(spdata), !is.null(nvec))
  d <- length(nvec)
  if (ncol(lags) != d) {
    stop(sprintf("ncol(lags) (%d) must equal length(dim(spdata)) (%d)", ncol(lags), d))
  }

  Rrad <- max(sqrt(rowSums(lags^2)))
  lagsteps <- lags # nh x d, assumed integer-valued

  # Determine valid interior grid ranges to prevent out-of-bounds indexing
  ranges <- lapply(nvec, function(n) {
    lo <- ceiling(1 + Rrad); hi <- floor(n - Rrad)
    if (hi < lo) integer(0) else lo:hi
  })

  spMaximizer <- array(NA_integer_, dim = nvec)
  if (any(vapply(ranges, length, integer(1)) == 0)) {
    return(spMaximizer)  # no interior points for this lags_list/delta
  }

  # Calculate column-major strides for linear indexing
  strides <- c(1, cumprod(nvec)[-d])
  lagoffsets <- as.vector(lagsteps %*% strides)

  idxgrid <- as.matrix(do.call(expand.grid, ranges))
  baselin <- as.vector((idxgrid - 1) %*% strides) + 1L

  # Iterate over valid interior points using the linear index
  for (p in seq_len(nrow(idxgrid))) {
    base <- baselin[p]
    z0 <- spdata[[base]]                 # single linear index -> raw object
    zlagged <- spdata[base + lagoffsets] # linear indices -> stacked sub-list

    obj_vals <- objfunc(z0, zlagged)

    best <- if (minimize) min(obj_vals) else max(obj_vals)
    maximizers <- which(obj_vals == best)

    # Handle ties randomly to prevent directional bias
    spMaximizer[base] <- if (length(maximizers) > 1) sample(maximizers, 1) else maximizers
  }

  spMaximizer
}


#' Compute deviation from uniformity test statistic
#'
#' @description
#' Calculates the test statistic for deviation from a discrete uniform distribution
#' over the spatial index field.
#'
#' @param spIndex Matrix or array of categorical indices (integers from 1 to \code{nh}).
#' @param nh Integer. The total number of possible categorical states (indices).
#'
#' @return A list containing:
#'   \item{Tstat}{The aggregated scalar test statistic.}
#'   \item{Tstatv}{The vector of normalized deviations for each category.}
#'   \item{nObs}{The total number of valid observations.}
#'
#' @keywords internal
#' @noRd
deviationFromUnif <- function(spIndex, nh) {

  # Tabulate occurrences of each category state
  indexCnts <- table(factor(spIndex, levels = 1:nh))
  nObs <- sum(indexCnts)
  unifCnts <- rep(nObs/nh, nh)

  # Compute normalized deviations
  Tstatv <- 1/sqrt(nObs) * (indexCnts - unifCnts)
  Tstat <- sum(Tstatv^2)

  return(list(Tstat = Tstat,
              Tstatv = Tstatv,
              nObs = nObs))
}


#' Estimate Variance in  Local (Block) Proportions Across Arbitrary-Dimensional Categorical Fields
#'
#' @description
#' Computes empirical variance in block proportion vectors for categorical fields across $D$ spatial
#' dimensions without explicitly materializing sparse one-hot indicator arrays in memory.
#' Accepts either a single spatial field or an $S$-stacked multi-layer field (e.g., across
#' distinct directions, variables, or time steps). Stacked encodings are concatenated
#' along the categories axis, enabling joint cross-covariance estimation across all
#' $S x n_h$ indicator components.
#'
#' @param indexField An array of category indices with values in \code{1:nh} (where \code{NA}
#'   denotes unobserved boundary regions). Can be a $D$-dimensional array
#'   \eqn{(N_1 x \dots x N_D)} for a single field, or a $(D+1)$-dimensional array
#'   \eqn{(N_1 x \dots x N_D x S)} containing $S$ stacked fields sharing the
#'   same $n_h$ categorical scope.
#' @param nh Integer scalar. The total number of categorical states (e.g., \code{nrow(lags_list)}).
#' @param window_dims Integer vector defining the dimensions of the moving window.
#' @param lims_mat A $D x 2$ matrix defining the spatial bounds for evaluation.
#' @param overlap_dims Integer vector defining the step overlap along each spatial axis.
#'   Defaults to \code{0} across all dimensions.
#'
#' @return A list containing:
#'   \item{sig}{The estimated long-run covariance matrix.}
#'   \item{mu}{The global spatial block proportions.}
#'   \item{n_windows}{The total number of subwindows evaluated.}
#'
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' # Placeholder for examples if this function remains exported
#' }
longrunvariance_from_indexfield <- function(indexField,
                                            nh,
                                            window_dims,
                                            lims_mat,
                                            overlap_dims = rep(0L, length(window_dims))) {

  # Assert array dimensions and safely extract spatial structures
  d_orig <- dim(indexField)
  if (is.null(d_orig)) {
    stop("`indexField` must be an array.", call. = FALSE)
  }

  total_dims <- length(d_orig)
  n_spatial  <- length(window_dims)

  # Safely handle the trailing dimension for stacked fields (S)
  if (total_dims == n_spatial) {
    S <- 1L
    dim(indexField) <- c(d_orig, 1L)
  } else if (total_dims == n_spatial + 1L) {
    S <- d_orig[total_dims]
  } else {
    stop(sprintf(
      "Dimension mismatch: `window_dims` has length %d, but `indexField` has %d dimensions.",
      n_spatial, total_dims
    ), call. = FALSE)
  }

  if (!is.matrix(lims_mat) || nrow(lims_mat) != n_spatial || ncol(lims_mat) != 2L) {
    stop("`lims_mat` must be a D x 2 matrix of spatial bounds.", call. = FALSE)
  }

  # Generate full spatial coordinate vectors along each spatial axis based on limits
  coords_list <- lapply(seq_len(n_spatial), function(dim_idx) {
    lims_mat[dim_idx, 1L]:lims_mat[dim_idx, 2L]
  })

  axis_lengths <- vapply(coords_list, length, integer(1))
  step_dims    <- window_dims - overlap_dims

  if (any(step_dims <= 0L)) {
    stop("`window_dims` must be strictly greater than `overlap_dims` across all axes.", call. = FALSE)
  }

  # Establish the number of sliding steps available per axis
  n_lags <- floor((axis_lengths - overlap_dims) / step_dims)

  if (any(n_lags < 1L)) {
    stop("Moving window dimensions exceed spatial domain boundaries.", call. = FALSE)
  }

  # Compute the global spatial block proportion as a baseline
  muhat <- block_proportions(indexField, coords_list, nh, S)
  K     <- length(muhat)

  # Generate multi-dimensional step combination indices (Cartesian product)
  lag_sequences <- lapply(n_lags, seq_len)
  grid_indices  <- as.matrix(expand.grid(lag_sequences))

  n_total_windows <- nrow(grid_indices)
  window_vol      <- prod(window_dims)

  # Pre-calculate coordinate sub-windows for every lag step across every dimension
  windows_by_axis <- lapply(seq_len(n_spatial), function(dim_idx) {
    w_len    <- window_dims[dim_idx]
    s_step   <- step_dims[dim_idx]
    axis_vec <- coords_list[[dim_idx]]

    lapply(seq_len(n_lags[dim_idx]), function(step_i) {
      start_idx <- (step_i - 1L) * s_step + 1L
      axis_vec[start_idx:(start_idx + w_len - 1L)]
    })
  })

  # Execute vectorized subwindow traversal mapping grid blocks to deviations
  dmu_list <- apply(grid_indices, 1L, function(step_vec) {
    current_subwindow_coords <- lapply(seq_len(n_spatial), function(dim_idx) {
      windows_by_axis[[dim_idx]][[step_vec[dim_idx]]]
    })

    muhat_window <- block_proportions(indexField, current_subwindow_coords, nh, S)
    muhat_window - muhat
  }, simplify = FALSE)

  # Accumulate cross-product covariance terms and scale by window DOF
  cov_acc <- Reduce(`+`, lapply(dmu_list, function(dmu) outer(dmu, dmu)))
  cov_est <- cov_acc * (window_vol / (n_total_windows - 1L))

  list(
    sig = cov_est,
    mu  = muhat,
    n_windows = n_total_windows
  )
}

#' Calculate interior valid range
#'
#' @description
#' Determines the safe interior indexing limits of a grid given a maximum boundary radius.
#'
#' @param nvec Integer vector representing the dimensions of the grid.
#' @param Rmax Numeric. The maximum radius/distance of the lag boundaries.
#'
#' @return A list containing:
#'   \item{lo}{Integer vector of lower bounds.}
#'   \item{hi}{Integer vector of upper bounds.}
#'   \item{empty}{Logical indicating if the resulting interior range is completely empty.}
#'
#' @keywords internal
#' @noRd
interior_range <- function(nvec, Rmax) {
  d <- length(nvec)
  lo <- integer(d)
  hi <- integer(d)
  empty <- FALSE
  for (i in seq_len(d)) {
    lo1 <- ceiling(1 + Rmax)
    hi1 <- floor(nvec[i] - Rmax)
    lo[i] <- lo1
    hi[i] <- hi1
    if (hi[i] < lo[i]) empty <- TRUE
  }
  list(lo = lo, hi = hi, empty = empty)
}

#' Relative index range (into xv or yv) of the b-th block along one axis.
#'
#' @param b Integer block index.
#' @param step Integer step size.
#' @param width Integer window width.
#'
#' @return An integer vector representing the sequential indices of the block.
#'
#' @keywords internal
#' @noRd
window_range <- function(b, step, width) {
  ((b - 1) * step + 1):((b - 1) * step + width)
}

#' Compute Empirical Block Proportions for D-Dimensional Categorical Fields
#'
#' @description
#' Calculates the normalized frequency of categorical states within a designated
#' coordinate sub-window.
#'
#' @param indexField An array of spatial category indices with values in \code{1:nh[s]}.
#'   Can be $D$-dimensional \eqn{(N_1 x \dots x N_D)} or $(D+1)$-dimensional
#'   \eqn{(N_1 x \dots x N_D x S)}.
#' @param coords_list A \code{list} of length $D$ containing integer coordinate vectors
#'   defining the spatial window indices along each spatial dimension.
#' @param nh An integer vector of length $S$ (or scalar if $S = 1$) specifying
#'   the number of category bins for each stacked layer.
#' @param S Integer scalar. The total number of stacked indicator layers.
#'
#' @return A numeric vector of length \eqn{\sum_{s=1}^S n_{h, s}} containing the
#'   concatenated empirical proportions across all $S$ layers.
#'
#' @keywords internal
#' @export
#'
#' @examples
#' \dontrun{
#' # Placeholder for examples if this function remains exported
#' }
block_proportions <- function(indexField, coords_list, nh, S) {

  # Structural Assertions
  if (!is.list(coords_list)) {
    stop("`coords_list` must be a list of integer spatial index vectors.", call. = FALSE)
  }

  if (length(nh) == 1L && S > 1L) {
    nh <- rep.int(nh, S)
  }

  # Setup dynamic hyper-slice extraction arguments
  slice_args <- c(list(indexField), coords_list)

  if (S > 1L || length(dim(indexField)) > length(coords_list)) {
    slice_args <- c(slice_args, list(seq_len(S)))
  }

  # Extract the multidimensional subwindow
  subwindow <- do.call(`[`, c(slice_args, list(drop = FALSE)))

  # Vectorize layer-wise binning via matrix reshaping
  # Each column represents the spatial block for layer s
  subwindow_mat <- matrix(subwindow, ncol = S)

  props <- numeric(sum(nh))

  # Establish cumulative offsets to map results back to a 1D vector
  offsets_end   <- cumsum(nh)
  offsets_start <- offsets_end - nh + 1L

  # Evaluate proportions column-wise
  for (s in seq_len(S)) {
    vals  <- subwindow_mat[, s]
    denom <- sum(!is.na(vals))

    if (denom > 0L) {
      props[offsets_start[s]:offsets_end[s]] <- tabulate(vals, nbins = nh[s]) / denom
    } else {
      # Fallback to zeros if the window contains strictly NAs
      props[offsets_start[s]:offsets_end[s]] <- 0.0
    }
  }

  props
}
