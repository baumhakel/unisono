
# tie-breaking was not tested. do so for binary field.



# spdata: 2-dimensional R array
# generated from gaussian random field


# generate data -----------------------------------------------------------


set.seed(1)
rf_dat2d <- geoR::grf(n = 250, nsim=2, grid = expand.grid(seq(0, 50, length.out = 50), seq(0, 50, length.out = 50)), cov.model = "exponential", cov.pars = c(1, 1), nugget = 0.1)
rf_dat3d <- geoR::grf(n = 13^3, nsim=2, grid = expand.grid(1:13, 1:13, 1:13), cov.model = "exponential", cov.pars = c(1, 2), nugget = 0.1)

# array fields

# scalar field, dummy dim
spd.asd <- array(rf_dat2d$data[,1], dim = c(50, 50, 1))
spd.asd3 <- array(rf_dat3d$data[,1], dim = c(13, 13, 13, 1))
# scalar field, no dummy dim
spd.as <- array(rf_dat2d$data[,1], dim = c(50, 50))
spd.as3 <- array(rf_dat3d$data[,1], dim = c(13, 13, 13))
# multivariate field
spd.am <- array(rf_dat2d$data, dim = c(50, 50, 2))
spd.am3 <- array(rf_dat3d$data, dim = c(13, 13, 13, 2))

image(spd.asd[,,1], axes = F)
image(spd.as, axes = F)
image(spd.am[,,1], axes = F)
image(spd.am[,,2], axes = F)

image(spd.as3[,,1], axes = F)
image(spd.am3[,,1,1], axes = F)
image(spd.am3[,,1,2], axes = F)

# list fields

# scalar field
spd.ls <- as.list(rf_dat2d$data[,1])
dim(spd.ls) <- c(50, 50)
spd.ls3 <- as.list(rf_dat3d$data[,1])
dim(spd.ls3) <- c(13, 13, 13)

# multivariate field
spd.lm <- lapply(1:nrow(rf_dat2d$data), function(i) rf_dat2d$data[i,])
dim(spd.lm) <- c(50, 50)
spd.lm3 <- lapply(1:nrow(rf_dat3d$data), function(i) rf_dat3d$data[i,])
dim(spd.lm3) <- c(13, 13, 13)

# check whether assignment worked
list2mat <- function(spd.l, dim, entry = NA) {
  spd.mat <- matrix(NA, nrow = dim[1], ncol = dim[2])
  for (i in 1:dim[1]) {
    for (j in 1:dim[2]) {
      if (is.na(entry)) {
        spd.mat[i,j] <- spd.l[[i,j]]
      } else {
        if (entry > 0) {
          spd.mat[i,j] <- spd.l[[i,j]][entry]
        } else {
          spd.mat[i,j] <- spd.l[[i,j]][[-entry]]
        }
      }
    }
  }
  return(spd.mat)
}

list2mat3 <- function(spd.l, dim, entry = NA) {
  spd.mat <- array(NA, dim = c(dim[1], dim[2], dim[3]))
  for (i in 1:dim[1]) {
    for (j in 1:dim[2]) {
      for (k in 1:dim[3]) {
        if (is.na(entry)) {
          spd.mat[i,j,k] <- spd.l[[i,j,k]]
        } else {
          if (entry > 0) {
            spd.mat[i,j,k] <- spd.l[[i,j,k]][entry]
          } else {
            spd.mat[i,j,k] <- spd.l[[i,j,k]][[-entry]]
          }
        }
      }
    }
  }
  return(spd.mat)
}

image(spd.as, axes = F)
image(list2mat(spd.ls, dim(spd.ls)), axes = F)

image(spd.am[,,1], axes = F)
image(list2mat(spd.lm, dim(spd.lm), entry = -1), axes = F)
image(spd.am[,,2], axes = F)
image(list2mat(spd.lm, dim(spd.lm), entry = -2), axes = F)

image(spd.am3[,,1,1], axes = F)
image(list2mat3(spd.lm3, dim(spd.lm3), entry = -1)[,,1], axes = F)
image(spd.am3[,,1,2], axes = F)
image(list2mat3(spd.lm3, dim(spd.lm3), entry = -2)[,,1], axes = F)



# SpatialGridDataFrame, (1) transforms to array (2) transforms to list
#    -> add option: >1 col, all numeric (stack them)

# univariate
spd.sgs <- sp::SpatialGridDataFrame(
  grid = sp::GridTopology(cellcentre.offset = c(0.5, 0.5), cellsize = c(1, 1), cells.dim = c(50, 50)),
  data = data.frame(field1 = rf_dat2d$data[,1])
)
spd.sgs3 <- sp::SpatialGridDataFrame(
  grid = sp::GridTopology(cellcentre.offset = c(0.5, 0.5, 0.5), cellsize = c(1, 1, 1), cells.dim = c(13, 13, 13)),
  data = data.frame(field1 = rf_dat3d$data[,1])
)

# multivariate to array

spd.sgm <- sp::SpatialGridDataFrame(
  grid = sp::GridTopology(cellcentre.offset = c(0.5, 0.5), cellsize = c(1, 1), cells.dim = c(50, 50)),
  data = data.frame(field1 = rf_dat2d$data)
)
spd.sgm3 <- sp::SpatialGridDataFrame(
  grid = sp::GridTopology(cellcentre.offset = c(0.5, 0.5, 0.5), cellsize = c(1, 1, 1), cells.dim = c(13, 13, 13)),
  data = data.frame(field1 = rf_dat3d$data)
)
# to list
# univariate
spd.sgsl <- sp::SpatialGridDataFrame(
  grid = sp::GridTopology(cellcentre.offset = c(0.5, 0.5), cellsize = c(1, 1), cells.dim = c(50, 50)),
  data = data.frame(field1 = ifelse(rf_dat2d$data[,1] > 0, "a", "b"))
)

spd.sgsl3 <- sp::SpatialGridDataFrame(
  grid = sp::GridTopology(cellcentre.offset = c(0.5, 0.5, 0.5), cellsize = c(1, 1, 1), cells.dim = c(13, 13, 13)),
  data = data.frame(field1 = ifelse(rf_dat3d$data[,1] > 0, "a", "b"))
)

# multivariate
spd.sgml <- sp::SpatialGridDataFrame(
  grid = sp::GridTopology(cellcentre.offset = c(0.5, 0.5), cellsize = c(1, 1), cells.dim = c(50, 50)),
  data = data.frame(field1 = ifelse(rf_dat2d$data[,1] > 0, "a", "b"), field2 = ifelse(rf_dat2d$data[,2] > 0, "a", "b"))
)
spd.sgml3 <- sp::SpatialGridDataFrame(
  grid = sp::GridTopology(cellcentre.offset = c(0.5, 0.5, 0.5), cellsize = c(1, 1, 1), cells.dim = c(13, 13, 13)),
  data = data.frame(field1 = ifelse(rf_dat3d$data[,1] > 0, "a", "b"), field2 = ifelse(rf_dat3d$data[,2] > 0, "a", "b"))
)


spd.sgs.a <- .convert_sgdf(spd.sgs)
spd.sgs3.a <- .convert_sgdf(spd.sgs3)
spd.sgm.a <- .convert_sgdf(spd.sgm)
spd.sgm3.a <- .convert_sgdf(spd.sgm3)

spd.sgsl.l <- .convert_sgdf(spd.sgsl)
spd.sgsl3.l <- .convert_sgdf(spd.sgsl3)
spd.sgml.l <- .convert_sgdf(spd.sgml)
spd.sgml3.l <- .convert_sgdf(spd.sgml3)

spd.sgs.a[1,1]
spd.sgs3.a[1,1,1]
spd.sgm.a[1,1,]
spd.sgm3.a[1,1,1,]

spd.sgsl.l[[1,1]]
spd.sgsl3.l[[1,1,1]]
spd.sgml.l[[1,1]]
spd.sgml3.l[[1,1,1]]






# test whether isotropy test works for all cases:
# array/list; univ/multivariate; 2d/3d; list: numbers/strings
# -> 8 instances (feed each to uni_iso_grid, expect no error and unifIsoResult object)

#uni_iso_grid(spdata,my_objfunc)

# a,u,2
spd.as
# a,u,3
spd.as3
# a,m,2
spd.am
# a,m,3
spd.am3

# l,u,2
spd.ls
# l,u,3
spd.ls3
# l,m,2
spd.lm
# l,m,3
spd.lm3

# l,u,2, s
spd.sgsl.l
# l,u,3, s
spd.sgsl3.l
# l,m,2, s
spd.sgml.l
# l,m,3, s
spd.sgml3.l



test_that("uni_iso_grid works across all input variations (array/list, uni/multivariate, 2D/3D, string labels)", {
  # 1. Pair each test dataset instance with its respective objective function
  myobj_uv <- function(xv, yv) {
    return(ifelse(xv == yv, 0, 1))
  }

  myobj_mv <- function(x, yl) {
    return(sapply(yl, function(y) {sum(unlist(x) != unlist(y))}))
  }


  test_datasets <- list(
    # Array inputs (univariate / multivariate, 2D / 3D)
    "array, scalar, 2D" = list(
      spdata  = spd.as,
      objfunc = "normdiff"
    ),
    "array, scalar, 3D" = list(
      spdata  = spd.as3,
      objfunc = "normdiff"
    ),
    "array, multivariate, 2D" = list(
      spdata  = spd.am,
      objfunc = "normdiff"
    ),
    "array, multivariate, 3D" = list(
      spdata  = spd.am3,
      objfunc = "normdiff"
    ),

    # List inputs (numeric coordinates/lags)
    "list, scalar, 2D" = list(
      spdata  = spd.ls,
      objfunc = myobj_uv
    ),
    "list, scalar, 3D" = list(
      spdata  = spd.ls3,
      objfunc = myobj_uv
    ),
    "list, multivariate, 2D" = list(
      spdata  = spd.lm,
      objfunc = myobj_mv
    ),
    "list, multivariate, 3D" = list(
      spdata  = spd.lm3,
      objfunc = myobj_mv
    ),

    # List inputs (string label indexing)
    "SpatialGridDataFrame, scalar, 2D (string)" = list(
      spdata  = spd.sgsl,
      objfunc = myobj_uv
    ),
    "SpatialGridDataFrame, scalar, 3D (string)" = list(
      spdata  = spd.sgsl3,
      objfunc = myobj_uv
    ),
    "SpatialGridDataFrame, multivariate, 2D (string)" = list(
      spdata  = spd.sgml,
      objfunc = myobj_mv
    ),
    "SpatialGridDataFrame, multivariate, 3D (string)" = list(
      spdata  = spd.sgml3,
      objfunc = myobj_mv )
  )

  # 2. Iterate through each instance and test execution
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
        objfunc = instance$objfunc
      ),
      regexp = NA,
      info = paste0("Failed on dataset configuration: ", name)
    )

    # Assert expected S3 return class
    expect_s3_class(
      res,
      "unifIsoResult"
    )
  }
})

res <- uni_iso_grid(spd.as)
res <- uni_iso_grid(spd.sgm, lags_list = list(rbind(diag(1, 2), diag(-1, 2))))



# define objective function for list type


# must be able to take object (xv) and list of objects (yv) and return a numeric value
myobj <- function(xv, yv) {
  return(ifelse(xv == yv, 0, 1))
}

res <- uni_iso_grid(spd.sgsl, lags_list = list(rbind(diag(1, 2), diag(-1, 2))), objfunc = myobj)

myobj <- function(x, yl) {
  return(sapply(yl, function(y) {sum(unlist(x) != unlist(y))}))
}

res <- uni_iso_grid(spd.sgml, lags_list = list(rbind(diag(1, 2), diag(-1, 2))), objfunc = myobj)




delta <- 1
lags_list <- rbind(c(1, 0), c(0, 1), c(-1, 0), c(0, -1))


# check index optimization ------------------------------------------------
ts <- Sys.time()
res <- argmaxLagIndexSqDiffCpp(spdata, delta, lags_list)
print(Sys.time() - ts)
res <- argmaxLagIndexSqDiffCpp(spdata, delta, lags_list)



# compare to old version --------------------------------------------------
source("C:/Users/baujuc12/Nextcloud/papier/rcode/paper_simulations/R/unifTest.R")

ts <- Sys.time()
res2 <- get_argmax_indices(spdata, obj.sqnorm, delta = 1, lags = lags_list)
print(Sys.time() - ts)

res2$spMaximizer == res


# replicate spdata 10 times in each direction
spdata_rep <- array(rep(spdata, each = 10), dim = c(10, 10, 10))

isoUnifTestGrid(spdata, obj.sqnorm, delta = 1, lags_list = lags_list)


# 3d version: stack twisted spdata onto itself
spdata3 <- array(NA, dim = c(50,50, 2))
spdata3[,,1] <- spdata[,,1]
spdata3[,,2] <- t(spdata[,,1])

res3 <- argmaxLagIndexSqDiffCpp(spdata3, delta, lags_list)
res3

ts <- Sys.time()
res3 <- argmaxLagIndexSqDiffCpp(spdata3, delta, lags_list)
print(Sys.time() - ts)


ts <- Sys.time()
res3a <- get_argmax_indices(spdata3, obj.sqnorm, delta = 1, lags = lags_list)
print(Sys.time() - ts)


# 3d version: check with R objective function
res4 <- argmaxLagIndexCpp(spdata3, obj.sqnorm, delta = 1, lags = lags_list)
