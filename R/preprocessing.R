




#' Convert a SpatialGridDataFrame to a D-Dimensional Numeric Array
#'
#' @description
#' Reshapes the attribute data of a SpatialGridDataFrame into an array based
#' on the object's GridTopology metadata. Handles both scalar and multivariate fields.
#'
#' @param sgdf A \code{SpatialGridDataFrame} object from the \code{sp} package.
#'
#' @return A numeric array with spatial dimensions for scalar data,
#'   or an extended dimension for multivariate vector-valued observations.
#'
#' @keywords internal
#' @noRd
.sgdf_to_array <- function(sgdf) {

  # Validate input class

  if (!inherits(sgdf, "SpatialGridDataFrame")) {
    stop("Input `sgdf` must inherit from class 'SpatialGridDataFrame'.", call. = FALSE)
  }

  # Extract grid topology and attribute dimensions
  spatial_dims <- sgdf@grid@cells.dim
  n_spatial    <- length(spatial_dims)


  data_mat <- as.matrix(sgdf@data)
  m_vars   <- ncol(data_mat)

  # Reshape flat column-major data into array topology
  if (m_vars == 1L) {

    target_dims <- spatial_dims
  } else {

    target_dims <- c(spatial_dims, m_vars)
  }


  res_array <- array(data_mat, dim = target_dims)

  res_array
}








#' Convert a SpatialGridDataFrame to a D-Dimensional List-Array
#'
#' @description
#' Reshapes attribute data from a SpatialGridDataFrame into a list-array with spatial
#' dimensions matching the object's GridTopology.

#'
#' @param sgdf A \code{SpatialGridDataFrame} object.
#' @param drop_single_col Logical. If \code{TRUE} and \code{@data} contains exactly one column,
#'   each cell of the list-array holds a scalar/object rather than a 1-element list.
#'   Default is \code{TRUE}.
#'
#' @return A \code{list} with a \code{dim} attribute set to spatial dimensions.
#'
#' @keywords internal
#' @noRd
.sgdf_to_list <- function(sgdf, drop_single_col = TRUE) {

  # Validate input and extract topology

  if (!inherits(sgdf, "SpatialGridDataFrame")) {
    stop("Input `sgdf` must inherit from class 'SpatialGridDataFrame'.", call. = FALSE)
  }

  spatial_dims <- sgdf@grid@cells.dim
  n_spatial    <- length(spatial_dims)
  n_cells      <- prod(spatial_dims)
  df_data      <- sgdf@data
  n_cols       <- ncol(df_data)

  # Extract cell observations


  if (n_cols == 1L && drop_single_col) {

    col_vals <- df_data[[1L]]

    if (is.list(col_vals)) {

      spdat_l <- col_vals
    } else {

      spdat_l <- as.list(col_vals)
    }
  } else {


    spdat_l <- lapply(seq_len(n_cells), function(i) {

      as.list(df_data[i, , drop = FALSE])
    })
  }

  # Assign spatial dimensions to the list


  dim(spdat_l) <- spatial_dims

  spdat_l
}

#' Route SpatialGridDataFrame to Array or List
#'
#' @description
#' Internal router that checks the data types within a SpatialGridDataFrame
#' and passes it to either the array or list conversion function.
#'
#' @param sgdf A \code{SpatialGridDataFrame}.
#' @return An array or a list-array.
#'
#' @keywords internal
#' @noRd
.convert_sgdf <- function(sgdf) {
  if (!inherits(sgdf, "SpatialGridDataFrame")) {
    stop("Input must inherit from class 'SpatialGridDataFrame'.", call. = FALSE)
  }

  if (all(sapply(sgdf@data, is.numeric))) {
    .sgdf_to_array(sgdf)
  } else {
    .sgdf_to_list(sgdf)
  }
}


#' Convert geodata object to Array
#'
#' Reshapes the attribute data of a geodata into an array based
#' on the object's coords entries. Handles both scalar and multivariate fields.
#'
#' @param gdt A \code{geodata} object from the \code{geoR} package.
#'
#' @return A numeric array with spatial dimensions for scalar data,
#'   or an extended dimension for multivariate vector-valued observations.
#'
#' @keywords internal
#' @noRd
.convert_gdt <- function(gdt) {
  if (!inherits(gdt, "geodata")) {
    stop("Input must inherit from class 'geodata'.", call. = FALSE)
  }

  if (any((round(gdt$coords) - gdt$coords) > 1e-10)) {
    stop("Coordinates must be integer.", call. = FALSE)
  }

  gdt$coords <- round(gdt$coords)


  # Extract grid topology and attribute dimensions
  n_spatial    <- ncol(gdt$coords)
  spatial_dims <- apply(gdt$coords, MARGIN = 2, max)

  data_mat <- as.matrix(gdt$data)
  m_vars   <- ncol(data_mat)

  # Reshape flat column-major data into array topology
  if (m_vars == 1L) {

    target_dims <- spatial_dims
  } else {

    target_dims <- c(spatial_dims, m_vars)
  }


  res_array <- array(data_mat, dim = target_dims)

  res_array
}

#' Validate Inputs for Isotropy Grid Testing
#'
#' @description
#' Performs checks and standardization on inputs before they are passed
#' to the computational engine.
#'
#' @keywords internal
#' @noRd
.validate_iso_inputs <- function(spdata, objfunc, minimize, lags_list, window_dims, overlap_dims) {
  spatial_dims <- dim(spdata)
  if (is.null(spatial_dims))
    stop("`spdata` must have valid spatial dimensions (`dim(spdata)` cannot be NULL).", call. = FALSE)


  if (!is.function(objfunc)) {
    if (is.list(spdata) || objfunc != "normdiff") {
      stop("`objfunc` must be a valid function accepting field observation pairs.", call. = FALSE)
    }
  }

  # Standardize lags_list to a list of matrices
  if (is.null(lags_list)) {
    ds <- length(spatial_dims) - ifelse(spatial_dims[length(spatial_dims)] == 1, 1, 0)
    lags_list <- list(rbind(diag(1, ds), diag(-1, ds)))
  }

  if (!is.list(lags_list) && !is.matrix(lags_list))
    stop("`lags_list` must be a list of matrices or a single matrix.", call. = FALSE)

  if (is.matrix(lags_list))
    lags_list <- list(lags_list)


  dh <- ncol(lags_list[[1]])
  if (dh < 2) {
    stop("domain (lags) must be at least 2-dimensional")
  }

  # Enforce matching dimensions between data and lags
  if (!is.list(spdata)) {
    if (dh > length(dim(spdata)) || dh < length(dim(spdata))-1) {
      stop("domain (lags) must be d- or (d-1)-dimensional for d-dimensional data")
    }
  } else {
    if (dh != length(dim(spdata))) {
      stop("domain (lags) must be d-dimensional for d-dimensional data")
    }
  }

  # Validate symmetry and constancy of lag definitions
  for (i in seq_along(lags_list)) {
    lags <- lags_list[[i]]

    if (ncol(lags) != dh) {
      stop("lags_list dimension is not constant")
    }


    Rsqv <- rowSums(lags^2)
    if (any(abs(Rsqv - Rsqv[1]) > 1e-6)) {
      warning("lags_list rows are not all of the same length, test is not valid")
    } else {

      uqvals1 <- unique(abs(lags[1,]))
      for (j in seq_len(nrow(lags))[-1]) {
        uqvalsj <- unique(abs(lags[j,]))
        if (!all(uqvals1 %in% uqvalsj) || !all(uqvalsj %in% uqvals1)) {
          warning("lags_list rows are not all permutations of one another (up to sign), test may not be valid. check whether lag set is invariant under orthogonal transformations.")
        }
      }
    }
  }

  # Initialize missing window and overlap dimensions

  if (is.null(window_dims)) {
    window_dims <- rep(2L, dh)
  }

  if (is.null(overlap_dims)) {
    overlap_dims <- rep(0L, dh)
  }

  # Validate window and overlap structures
  if (length(window_dims) != dh) {
    stop(sprintf("`window_dims` must be a vector of length %d (dh).", dh), call. = FALSE)
  }
  if (length(overlap_dims) != dh) {
    stop(sprintf("`overlap_dims` must be a vector of length %d (dh).", dh), call. = FALSE)
  }
  if (any(window_dims <= overlap_dims)) {
    stop("`window_dims` must be strictly greater than `overlap_dims` across all axes.", call. = FALSE)
  }


  list(
    lags_list = lags_list,
    window_dims = window_dims,
    overlap_dims = overlap_dims
  )
}



#' Internal Multi-Assignment Operator
#'
#' @description
#' Unpacks a list to multiple variables.
#'
#' @keywords internal
#' @noRd
`%<-%` <- function(lvalues, rvalues) {
  targets <- as.character(substitute(lvalues))[-1L]
  for (i in seq_along(targets)) {
    assign(targets[i], rvalues[[i]], envir = parent.frame())
  }
}

#' Get Information from Lags List
#'
#' @description
#' Extracts the number of lags and norm radius for each lag set.
#'
#' @keywords internal
#' @noRd
get_lags_list_info <- function(lags_list) {
  L <- length(lags_list)
  nhv <- numeric(L)
  Rv <- numeric(L)

  for (i in seq_along(lags_list)) {
    lags <- lags_list[[i]]
    nhv[i] <- nrow(lags)
    Rv[i] <- sqrt(sum(lags[1,]^2))
  }

  return(list(nhv = nhv, Rv = Rv))
}
