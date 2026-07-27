#' Isotropy test for function-valued random fields on an n-dimensional grid
#'
#' @description
#' Performs a uniformity-based isotropy test for spatial data on a regular grid based on
#' Baumhakel, Hörmann, Neumann (2026+).
#' The function accepts matrices, multidimensional arrays, `SpatialGridDataFrame`, and `geodata` objects.
#' Depending on the data type, it evaluates the given objective function over specified spatial lags
#' at every point in the field to find maximizing directions. The proportion of maximizers
#' are compared to the uniform distribution. to determine whether the field exhibits directional dependence (anisotropy).
#'
#' @param spdata A spatial data object containing the random field observed on a rectangular d-dimensional grid.
#'   Can be a `matrix`, `array`, a `SpatialGridDataFrame`, or a `geodata` object.
#'   In the case of a matrix or array with scalar entries, where the last dimension
#'   represents the coordinate index of a multivariate field. The array may also contain
#'   general objects (lists).
#' @param objfunc Character string or function. The objective function to be evaluated
#'   over directions to obtain the argmax. Must be vectorized. Default is `"normdiff"`, which
#'   computes the squared Euclidean norm of the difference and is implemented in C++ for efficiency.
#'   This works only for scalar or multivariate numeric data.
#' @param minimize Logical. Should the objective function be minimized instead of maximized?
#'   Default is `FALSE` (maximize).
#' @param lags_list A list of matrices, where each matrix represents a set of lag vectors
#'   (each row is a lag vector in the scale of the grid step size). If not specified,
#'   defaults to a single lag set of unit vectors in each spatial dimension and their negatives.
#'   Lag vectors in the same matrix need to be of the same norm and fulfill the condition
#'   outlined in Baumhakel, Hörmann, Neumann (2026+).
#' @param window_dims Integer vector. Dimensions of the moving window used to estimate
#'   the asymptotic long-run covariance matrix. If not specified, defaults to a vector of 2's.
#' @param overlap_dims Integer vector. Dimensions of overlap for the moving window. If not specified,
#'   defaults to a vector of zeros.
#'
#' @return An object of class `unifIsoResult` containing:
#'   \item{statistic}{The overall test statistic value.}
#'   \item{stat_vec}{Vector of test statistics for each lag vector.}
#'   \item{p_value}{The p-value of the test.}
#'   \item{moments}{List containing the estimated proportions of maximizers and long-run covariance matrix (`sig`).}
#'   \item{eigenvalues}{Eigenvalues of the estimated covariance matrix.}
#'   \item{n_obs}{Vector of the number of observations used for each lag set (edge of the domain cannot be used).}
#'   \item{lags_list}{The used list of lags.}
#'   \item{window_dims}{The used window dimensions.}
#'   \item{overlap_dims}{The used overlap dimensions.}
#'
#' @export
#'
#' @example man/examples/ex_uni_iso_grid.R
uni_iso_grid <- function(spdata,
                         objfunc = "normdiff",
                         minimize = FALSE,
                         lags_list = NULL,
                         window_dims = NULL,
                         overlap_dims = NULL) {
  # Dispatch to the appropriate S3 method based on the class of spdata
  UseMethod("uni_iso_grid", spdata)
}

#' @rdname uni_iso_grid
#' @export
uni_iso_grid.matrix <- function(spdata,
                                objfunc = "normdiff",
                                minimize = FALSE,
                                lags_list = NULL,
                                window_dims = NULL,
                                overlap_dims = NULL) {

  # Validate and extract input parameters using zeallot's multi-assignment
  c(lags_list, window_dims, overlap_dims) %<-% .validate_iso_inputs(
    spdata, objfunc, minimize, lags_list, window_dims, overlap_dims)

  # Check if the field is a simple matrix structure (not a list of objects)
  if (!is.list(spdata)) {
    dtot <- dim(spdata)
    dh <- ncol(lags_list[[1]])

    # If the field is a scalar field without a singleton dimension for the variable,
    # append a 1 to the dimensions so the engine can process it consistently.
    if (length(dtot) == dh) {
      spdata <- array(spdata, dim = c(dtot, 1))
    }
  }

  # Forward the processed inputs to the internal computational engine
  uni_iso_grid_engine(spdata, objfunc, minimize, lags_list, window_dims, overlap_dims)
}

#' @rdname uni_iso_grid
#' @export
uni_iso_grid.array <- uni_iso_grid.matrix


#' @rdname uni_iso_grid
#' @export
uni_iso_grid.geodata <- function(spdata,
                                 objfunc = "normdiff",
                                 minimize = FALSE,
                                 lags_list = NULL,
                                 window_dims = NULL,
                                 overlap_dims = NULL)
{

  # Extract the underlying grid data array from the geodata object
  spdata <- .convert_gdt(spdata)

  # Proceed with the generic pipeline now that we have an array/matrix
  uni_iso_grid(spdata, objfunc, minimize, lags_list, window_dims, overlap_dims)
}



#' @rdname uni_iso_grid
#' @export
uni_iso_grid.SpatialGridDataFrame <- function(spdata,
                                              objfunc = "normdiff",
                                              minimize = FALSE,
                                              lags_list = NULL,
                                              window_dims = NULL,
                                              overlap_dims = NULL)
{

  # Extract the underlying grid data array (or list) from the sp object
  spdata <- .convert_sgdf(spdata)

  # Proceed with the generic pipeline now that we have an array/matrix
  uni_iso_grid(spdata, objfunc, minimize, lags_list, window_dims, overlap_dims)
}


#' @rdname uni_iso_grid
#' @export
uni_iso_grid.default <- function(spdata,
                                 objfunc = "normdiff",
                                 minimize = FALSE,
                                 lags_list = NULL,
                                 window_dims = NULL,
                                 overlap_dims = NULL)
{
  # Fail gracefully if an unsupported object class is passed, pasting all inherited classes
  stop(
    sprintf(
      "`uni_iso_grid()` does not support objects of class: %s",
      paste(class(spdata), collapse = ", ")
    ),
    call. = FALSE
  )
}

#' Internal computation engine for uniformity-based isotropy testing on grids
#'
#' @description
#' Handles the core mathematical operations: obtaining argmax indices, calculating
#' deviations from uniformity, estimating long-run variance, and estimating quantiles
#' of the asymptotic distribution.
#'
#' @keywords internal
#' @noRd
uni_iso_grid_engine <- function(spdata,
                                objfunc,
                                minimize,
                                lags_list = NULL,
                                window_dims = NULL,
                                overlap_dims = NULL) {

  dtot <- dim(spdata)
  dh <- ncol(lags_list[[1]])
  ds <- dtot[1:dh] # Spatial dimensions only

  L <- length(lags_list)
  nhv <- numeric(L)
  Rv <- numeric(L)

  # Extract lag information
  c(nhv, Rv) %<-% get_lags_list_info(lags_list)

  # Initialize accumulators and storage arrays for computing maximizing indices and errors
  nObsv <- numeric(L)
  err_sum <- 0
  err_stack <- numeric(sum(nhv))

  # index_stack allocates space for the argmax indices across all lags
  index_stack <- array(NA, dim = c(dim(spdata)[1:dh], L))
  lims_mat <- matrix(c(rep(0,dh), ds), nrow = dh, ncol = 2)

  current_idx <- 1
  for (i in seq_along(lags_list)) {

    # Calculate the field of argmax indices for the i-th lag set
    index_field <- get_argmax_indices(spdata, objfunc, lags_list[[i]], minimize)

    # index_stack is a dh-dimensional array (plus the lag dimension).
    # We dynamically construct the subsetting expression to assign index_field
    # to the last dimension (i), regardless of the spatial dimension (dh).
    inds <- rep(alist( , )[1], dh + 1)
    inds[[dh + 1]] <- i
    index_stack <- do.call(`[<-`, c(list(index_stack), inds, list(value = index_field)))

    # Compute deviations from uniformity
    unif_err <- deviationFromUnif(index_field, nh = nhv[i])
    err_sum <- err_sum + unif_err$Tstat
    nObsv[i] <- unif_err$nObs

    # Store stacked errors.
    err_stack[current_idx:(current_idx + nhv[i] - 1)] <- unif_err$Tstatv
    current_idx <- current_idx + nhv[i]

    # Calculate valid interior limits to avoid boundary effects in covariance estimation
    optrange <- interior_range(ds, Rv[i])

    if (optrange$empty) {
      stop(paste0("The lag set ", i, " is too large for the given data dimensions. Please consider supplying a custom lag set or reducing lag lengths."), call. = FALSE)
    }

    # Update global bounding box (lims_mat) for spatial evaluation
    lims_mat[, 1] <- pmax(lims_mat[1:dh, 1], optrange$lo)
    lims_mat[, 2] <- pmin(lims_mat[1:dh, 2], optrange$hi)
  }

  # Estimate long-run covariance matrix using the stacked indices
  sighat <- longrunvariance_from_indexfield(index_stack, nhv, window_dims,lims_mat,overlap_dims)

  # Compute eigenvalues and the corresponding p-value via Davies' method
  lambda <- eigen(sighat$sig)$values
  pval <- CompQuadForm::davies(err_sum, lambda)$Qq

  # Return results as a custom S3 object
  new_unifIsoResult(
    statistic   = err_sum,
    stat_vec    = err_stack,
    p_value     = pval,
    moments     = sighat,
    eigenvalues = lambda,
    n_obs       = nObsv,
    lags_list   = lags_list,
    window_dims = window_dims,
    overlap_dims = overlap_dims
  )
}
