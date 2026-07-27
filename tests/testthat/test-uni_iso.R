#' @title Unit tests for Core Isotropy Grid Functionality
#' @description
#' Verifies that the `uni_iso_grid` function correctly processes various spatial
#' data structures (arrays, lists, SpatialGridDataFrames) across varying
#' dimensions (2D, 3D) and appropriately handles multivariate fields and custom
#' objective functions.

skip_if_not_installed("geoR")
skip_if_not_installed("sp")
skip_if_not_installed("withr")

# Setup mock spatial data using the geoR package
rf_dat2d <- withr::with_seed(1, {
  geoR::grf(n = 2500, nsim=2,
                      grid = expand.grid(1:50, 1:50),
                      cov.model = "exponential", cov.pars = c(1, 1), nugget = 0.1,
                      messages = FALSE)
})
rf_dat3d <- withr::with_seed(1, {
  geoR::grf(n = 13^3, nsim=2,
                      grid = expand.grid(1:13, 1:13, 1:13),
                      cov.model = "exponential", cov.pars = c(1, 2), nugget = 0.1,
                      messages = FALSE)
})

rf_dat2ds <- rf_dat2d
rf_dat2ds$data <- rf_dat2ds$data[,1]

rf_dat3ds <- rf_dat3d
rf_dat3ds$data <- rf_dat3ds$data[,1]

# --- Array field fixtures ---

# Scalar field with a dummy dimension
spd.asd <- array(rf_dat2d$data[,1], dim = c(50, 50, 1))
spd.asd3 <- array(rf_dat3d$data[,1], dim = c(13, 13, 13, 1))

# Scalar field without a dummy dimension
spd.as <- array(rf_dat2d$data[,1], dim = c(50, 50))
spd.as3 <- array(rf_dat3d$data[,1], dim = c(13, 13, 13))

# Multivariate field
spd.am <- array(rf_dat2d$data, dim = c(50, 50, 2))
spd.am3 <- array(rf_dat3d$data, dim = c(13, 13, 13, 2))

# --- List field fixtures ---

# Scalar field
spd.ls <- as.list(rf_dat2d$data[,1])
dim(spd.ls) <- c(50, 50)
spd.ls3 <- as.list(rf_dat3d$data[,1])
dim(spd.ls3) <- c(13, 13, 13)

# Multivariate field
spd.lm <- lapply(1:nrow(rf_dat2d$data), function(i) rf_dat2d$data[i,])
dim(spd.lm) <- c(50, 50)
spd.lm3 <- lapply(1:nrow(rf_dat3d$data), function(i) rf_dat3d$data[i,])
dim(spd.lm3) <- c(13, 13, 13)

# --- SpatialGridDataFrame fixtures ---
# Tests conversion pathways:
# 1. Transforms to array (all numeric)
# 2. Transforms to list (mixed/non-numeric types)
# -> add option: >1 col, all numeric (stack them)

# Univariate array transformation
spd.sgs <- sp::SpatialGridDataFrame(
  grid = sp::GridTopology(cellcentre.offset = c(0.5, 0.5), cellsize = c(1, 1), cells.dim = c(50, 50)),
  data = data.frame(field1 = rf_dat2d$data[,1])
)
spd.sgs3 <- sp::SpatialGridDataFrame(
  grid = sp::GridTopology(cellcentre.offset = c(0.5, 0.5, 0.5), cellsize = c(1, 1, 1), cells.dim = c(13, 13, 13)),
  data = data.frame(field1 = rf_dat3d$data[,1])
)

# Multivariate array transformation
spd.sgm <- sp::SpatialGridDataFrame(
  grid = sp::GridTopology(cellcentre.offset = c(0.5, 0.5), cellsize = c(1, 1), cells.dim = c(50, 50)),
  data = data.frame(field1 = rf_dat2d$data)
)
spd.sgm3 <- sp::SpatialGridDataFrame(
  grid = sp::GridTopology(cellcentre.offset = c(0.5, 0.5, 0.5), cellsize = c(1, 1, 1), cells.dim = c(13, 13, 13)),
  data = data.frame(field1 = rf_dat3d$data)
)

# Univariate list transformation (using strings to force list routing)
spd.sgsl <- sp::SpatialGridDataFrame(
  grid = sp::GridTopology(cellcentre.offset = c(0.5, 0.5), cellsize = c(1, 1), cells.dim = c(50, 50)),
  data = data.frame(field1 = ifelse(rf_dat2d$data[,1] > 0, "a", "b"))
)

spd.sgsl3 <- sp::SpatialGridDataFrame(
  grid = sp::GridTopology(cellcentre.offset = c(0.5, 0.5, 0.5), cellsize = c(1, 1, 1), cells.dim = c(13, 13, 13)),
  data = data.frame(field1 = ifelse(rf_dat3d$data[,1] > 0, "a", "b"))
)

# Multivariate list transformation
spd.sgml <- sp::SpatialGridDataFrame(
  grid = sp::GridTopology(cellcentre.offset = c(0.5, 0.5), cellsize = c(1, 1), cells.dim = c(50, 50)),
  data = data.frame(field1 = ifelse(rf_dat2d$data[,1] > 0, "a", "b"), field2 = ifelse(rf_dat2d$data[,2] > 0, "a", "b"))
)
spd.sgml3 <- sp::SpatialGridDataFrame(
  grid = sp::GridTopology(cellcentre.offset = c(0.5, 0.5, 0.5), cellsize = c(1, 1, 1), cells.dim = c(13, 13, 13)),
  data = data.frame(field1 = ifelse(rf_dat3d$data[,1] > 0, "a", "b"), field2 = ifelse(rf_dat3d$data[,2] > 0, "a", "b"))
)


# Define custom objective functions corresponding to the test dataset structures
myobj_uv <- function(xv, yv) {
  return(ifelse(xv == yv, 0, 1))
}

myobj_mv <- function(x, yl) {
  return(sapply(yl, function(y) {sum(unlist(x) != unlist(y))}))
}

test_that("uni_iso_grid works across all input variations (array/list, uni/multivariate, 2D/3D, string labels)", {

  test_datasets <- list(
    # Array inputs (univariate / multivariate, 2D / 3D)
    "array, scalar, 2D" = list(
      spdata  = spd.as,
      objfunc = "normdiff",
      ddim = 2
    ),
    "array, scalar, 3D" = list(
      spdata  = spd.as3,
      objfunc = "normdiff",
      ddim = 3
    ),
    "array, multivariate, 2D" = list(
      spdata  = spd.am,
      objfunc = "normdiff",
      ddim = 2
    ),
    "array, multivariate, 3D" = list(
      spdata  = spd.am3,
      objfunc = "normdiff",
      ddim = 3
    ),

    # List inputs (numeric coordinates/lags)
    "list, scalar, 2D" = list(
      spdata  = spd.ls,
      objfunc = myobj_uv,
      ddim = 2
    ),
    "list, scalar, 3D" = list(
      spdata  = spd.ls3,
      objfunc = myobj_uv,
      ddim = 3
    ),
    "list, multivariate, 2D" = list(
      spdata  = spd.lm,
      objfunc = myobj_mv,
      ddim = 2
    ),
    "list, multivariate, 3D" = list(
      spdata  = spd.lm3,
      objfunc = myobj_mv,
      ddim = 3
    ),

    # SpatialGridDataFrame inputs (numeric labels)
    "SpatialGridDataFrame, scalar, 2D" = list(
      spdata  = spd.sgs,
      objfunc = "normdiff",
      ddim = 2
    ),
    "SpatialGridDataFrame, scalar, 3D" = list(
      spdata  = spd.sgs3,
      objfunc = "normdiff",
      ddim = 3
    ),
    "SpatialGridDataFrame, multivariate, 2D" = list(
      spdata  = spd.sgm,
      objfunc = "normdiff",
      ddim = 2
    ),
    "SpatialGridDataFrame, multivariate, 3D" = list(
      spdata  = spd.sgm3,
      objfunc = "normdiff",
      ddim = 3
    ),

    # SpatialGridDataFrame inputs (string labels)
    "SpatialGridDataFrame, scalar, 2D (string)" = list(
      spdata  = spd.sgsl,
      objfunc = myobj_uv,
      ddim = 2
    ),
    "SpatialGridDataFrame, scalar, 3D (string)" = list(
      spdata  = spd.sgsl3,
      objfunc = myobj_uv,
      ddim = 3
    ),
    "SpatialGridDataFrame, multivariate, 2D (string)" = list(
      spdata  = spd.sgml,
      objfunc = myobj_mv,
      ddim = 2
    ),
    "SpatialGridDataFrame, multivariate, 3D (string)" = list(
      spdata  = spd.sgml3,
      objfunc = myobj_mv,
      ddim = 3
    ),

    # geodata inputs
    "geodata, scalar, 2D" = list(
      spdata  = rf_dat2ds,
      objfunc = "normdiff",
      ddim = 2
    ),
    "geodata, scalar, 3D" = list(
      spdata  = rf_dat3ds,
      objfunc = "normdiff",
      ddim = 3
    ),
    "geodata, multivariate, 2D" = list(
      spdata  = rf_dat2d,
      objfunc = "normdiff",
      ddim = 2
    ),
    "geodata, multivariate, 3D" = list(
      spdata  = rf_dat3d,
      objfunc = "normdiff",
      ddim = 3
    )
  )

  # Execute function sequentially across parameter combinations
  for (name in names(test_datasets)) {
    instance <- test_datasets[[name]]

    # Safeguard check to ensure dataset fixtures exist
    expect_false(
      is.null(instance$spdata),
      info = paste0("Test fixture spdata object for '", name, "' is NULL.")
    )


    # Assert no errors thrown during calculation
    expect_error(
      res <- uni_iso_grid(
        spdata  = instance$spdata,
        objfunc = instance$objfunc,
        lags_list = list(rbind(diag(1, instance$ddim), diag(-1, instance$ddim)))
      ),
      regexp = NA,
      info = paste0("Failed on dataset configuration: ", name)
    )

    # Assert expected S3 return class
    expect_s3_class(
      res,
      "unifIsoResult"
    )

    # Validate mathematical constraints of the returned p-value
    expect_true(
      is.numeric(res$p_value) && res$p_value >= 0 && res$p_value <= 1,
      info = paste0("p-value out of bounds for dataset configuration: ", name)
    )
  }
})

rf_dat2d <- withr::with_seed(1, {
  geoR::grf(n = 2500, nsim=1,
                      grid = expand.grid(1:50, 1:50),
                      cov.model = "exponential", cov.pars = c(1, 1), nugget = 0.1,
                      messages = FALSE)
})

spd.as <- array(rf_dat2d$data, dim = c(50, 50))

# Define multiple lag matrices
lags_list <- list(
  rbind(diag(1, 2), diag(-1, 2)),
  rbind(c(1, 0), c(0, 1))
)

# Custom objective function
obj.abs <- function(z0, zlagged) {
  diffs <- zlagged - matrix(rep(z0, nrow(zlagged)), nrow = nrow(zlagged), byrow = T)
  sqnorms <- rowSums(abs(diffs))
  return(sqnorms)
}

test_that("uni_iso_grid handles multiple lag matrices and custom objective functions", {


  expect_error(
    res <- uni_iso_grid(
      spdata = spd.as,
      objfunc = obj.abs,
      lags_list = lags_list
    ),
    regexp = NA
  )

  expect_s3_class(res, "unifIsoResult")
})


test_that("print method outputs correctly", {
  res <- uni_iso_grid(spdata = spd.as, objfunc = obj.abs, lags_list = lags_list)

  # expect_output captures the cat() and print() statements
  expect_output(print(res), "Isotropy test for random fields on a grid")
  expect_output(print(res), "Test statistic:")
  expect_output(print(res), "p-value:")
})


test_that("summary method returns expected data structures", {
  res <- uni_iso_grid(spdata = spd.as, objfunc = obj.abs, lags_list = lags_list)
  res_sum <- summary(res)

  expect_s3_class(res_sum, "summary.unifIsoResult")
  expect_true(is.numeric(res_sum$statistic))
  expect_true(is.logical(res_sum$reject))

  expect_output(print(res_sum), "Subsampling setup:")
  expect_output(print(res_sum), "Lag families tested")


  # Ensure the lag_table was generated correctly
  expect_s3_class(res_sum$lag_table, "unifIsoResultLagTable")
})


test_that("plot generates valid ggplot structure and data bindings", {
  res <- uni_iso_grid(spdata = spd.as, objfunc = obj.abs, lags_list = lags_list)

  # Ensure the plot method runs cleanly and returns a ggplot object
  p <- suppressWarnings(plot(res))
  expect_s3_class(p, "ggplot")
  expect_s3_class(p, "gg")

  # Verify that the underlying plot data is correctly inherited from compute_ci_table
  expect_true(is.data.frame(p$data))
  expect_true(all(c("set", "lags", "prop", "ci_lower", "ci_upper", "length") %in% names(p$data)))

  # Verify that facets are correctly applied over the lag sets
  expect_true(inherits(p$facet, "FacetWrap"))

  # Verify that the plot contains the expected geometric layers
  # (e.g., geom_rect for confidence intervals, geom_segment for the lollipop stems, geom_point)
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomRect" %in% layer_classes)
  expect_true("GeomSegment" %in% layer_classes)
  expect_true("GeomPoint" %in% layer_classes)


  # Same for lollipop plot
  p <- suppressWarnings(plot(res, force_lollipop = TRUE))
  expect_s3_class(p, "ggplot")
  expect_s3_class(p, "gg")
  expect_true(inherits(p$facet, "FacetWrap"))
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomRect" %in% layer_classes)
  expect_true("GeomSegment" %in% layer_classes)
  expect_true("GeomPoint" %in% layer_classes)
})
