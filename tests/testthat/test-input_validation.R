#' @title Unit tests for Input Validation
#' @description
#' Tests the internal `.validate_iso_inputs` function to ensure it correctly
#' catches structural errors, validates objective functions, enforces spatial
#' bounds, and triggers appropriate warnings for invalid lag geometries.

test_that(".validate_iso_inputs asserts valid spdata dimensions", {
  # Defensive stop when input data lacks dimensions
  expect_error(
    .validate_iso_inputs(
      spdata = list(1, 2, 3),
      objfunc = "normdiff",
      minimize = FALSE,
      lags_list = NULL,
      window_dims = NULL,
      overlap_dims = NULL
    ),
    "`spdata` must have valid spatial dimensions (`dim(spdata)` cannot be NULL).",
    fixed = TRUE
  )
})

test_that(".validate_iso_inputs enforces valid objfunc requirements", {
  spdata_list <- vector("list", 4)
  dim(spdata_list) <- c(2, 2)

  # Enforce function requirement when data is a list-array
  expect_error(
    .validate_iso_inputs(
      spdata = spdata_list,
      objfunc = "invalid_string_op",
      minimize = FALSE,
      lags_list = NULL,
      window_dims = NULL,
      overlap_dims = NULL
    ),
    "`objfunc` must be a valid function accepting field observation pairs",
    fixed = TRUE
  )

  # Enforce valid objfunc string on standard arrays (only 'normdiff' allowed)
  spdata_arr <- array(1:8, dim = c(2, 2, 2))
  expect_error(
    .validate_iso_inputs(
      spdata = spdata_arr,
      objfunc = "custom_invalid_op",
      minimize = FALSE,
      lags_list = NULL,
      window_dims = NULL,
      overlap_dims = NULL
    ),
    "`objfunc` must be a valid function",
    fixed = TRUE
  )
})

test_that(".validate_iso_inputs standardizes and validates lag sets", {
  spdata_2d <- array(rnorm(16), dim = c(4, 4))

  # Catch invalid lag configuration formats
  expect_error(
    .validate_iso_inputs(
      spdata = spdata_2d,
      objfunc = "normdiff",
      lags_list = "invalid_character_lag",
      minimize = FALSE,
      window_dims = NULL,
      overlap_dims = NULL
    ),
    "`lags_list` must be a list of matrices or a single matrix",
    fixed = TRUE
  )

  # Enforce minimum lag dimensionality
  oneD_lag <- matrix(c(1, -1), ncol = 1)
  expect_error(
    .validate_iso_inputs(
      spdata = spdata_2d,
      objfunc = "normdiff",
      lags_list = oneD_lag,
      minimize = FALSE,
      window_dims = NULL,
      overlap_dims = NULL
    ),
    "domain (lags) must be at least 2-dimensional",
    fixed = TRUE
  )

  # Enforce dimensional parity between spatial data and lag geometries
  lags_3d <- matrix(c(1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE)
  expect_error(
    .validate_iso_inputs(
      spdata = spdata_2d,
      objfunc = "normdiff",
      lags_list = lags_3d,
      minimize = FALSE,
      window_dims = NULL,
      overlap_dims = NULL
    ),
    "domain (lags) must be d- or (d-1)-dimensional",
    fixed = TRUE
  )
})

test_that(".validate_iso_inputs throws warnings for invalid lag geometry", {
  spdata_2d <- array(rnorm(16), dim = c(4, 4))

  # Trigger geometric integrity warning for sets containing different lag lengths
  unequal_norms_lag <- matrix(c(1, 0, 2, 0), ncol = 2, byrow = TRUE)
  expect_warning(
    .validate_iso_inputs(
      spdata = spdata_2d,
      objfunc = "normdiff",
      lags_list = unequal_norms_lag,
      minimize = FALSE,
      window_dims = NULL,
      overlap_dims = NULL
    ),
    "lags_list rows are not all of the same length",
    fixed = TRUE
  )

  # Trigger orthogonal invariance warning when lags are not permutations of one another
  non_perm_lag <- matrix(c(1, 0, 0.7071068, 0.7071068), ncol = 2, byrow = TRUE)
  expect_warning(
    .validate_iso_inputs(
      spdata = spdata_2d,
      objfunc = "normdiff",
      lags_list = non_perm_lag,
      minimize = FALSE,
      window_dims = NULL,
      overlap_dims = NULL
    ),
    "lags_list rows are not all permutations of one another",
    fixed = TRUE
  )
})

test_that(".validate_iso_inputs fills default dimensions and asserts spatial bounds", {
  spdata_2d <- array(rnorm(16), dim = c(4, 4))
  valid_lags <- matrix(c(1, 0, 0, 1, -1, 0, 0, -1), ncol = 2, byrow = TRUE)

  # Verify NULL dimensions correctly fall back to default vectors
  res <- .validate_iso_inputs(
    spdata = spdata_2d,
    objfunc = "normdiff",
    lags_list = valid_lags,
    minimize = FALSE,
    window_dims = NULL,
    overlap_dims = NULL
  )

  expect_type(res, "list")
  expect_equal(res$window_dims, c(2L, 2L))
  expect_equal(res$overlap_dims, c(0L, 0L))
  expect_equal(res$lags_list[[1]], valid_lags)

  # Enforce length match between dimension parameters and the lag domain
  expect_error(
    .validate_iso_inputs(
      spdata = spdata_2d,
      objfunc = "normdiff",
      lags_list = valid_lags,
      minimize = FALSE,
      window_dims = c(2, 2, 2),
      overlap_dims = NULL
    ),
    "`window_dims` must be a vector of length 2",
    fixed = TRUE
  )

  # Prevent infinite loops or memory errors by asserting strict window overlap rules
  expect_error(
    .validate_iso_inputs(
      spdata = spdata_2d,
      objfunc = "normdiff",
      lags_list = valid_lags,
      minimize = FALSE,
      window_dims = c(2, 2),
      overlap_dims = c(2, 2)
    ),
    "`window_dims` must be strictly greater than `overlap_dims`",
    fixed = TRUE
  )
})
