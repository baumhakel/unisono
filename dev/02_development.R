# ==============================================================================
# dev/02_development.R - Daily iteration cycle
# ==============================================================================

# 1. Update Roxygen documentation & NAMESPACE
devtools::document()
#Ctrl + Shift + D

devtools::install()

# 2. Load current state into memory (fast alternative to install/library)
devtools::load_all()
#Ctrl + Shift + L




# fix error that cpp is not available
Rcpp::compileAttributes()

# 2. Update NAMESPACE (ensures @useDynLib unisono, .registration = TRUE is present)
devtools::document()

# 3. Force-recompile C++ code and reload the package cleanly
pkgload::load_all(recompile = TRUE)



# check coverage of unittests
covr::package_coverage()

# or more informative:
cover_res <- covr::package_coverage()
as.data.frame(cover_res)
covr::zero_coverage(cover_res)



# 3. Run unit tests
devtools::test()
#Ctrl + Shift + T

# 4. Check package health (CRAN standards)
devtools::check()
#Ctrl + Shift + E


devtools::check(cran = TRUE)


# linux
# Setup R-hub configuration in your repo once
rhub::rhub_setup()
# later
# Submit checks across multiple platforms
rhub::rhub_check()
# rhub will run you through.


# R development version for windows
devtools::check_win_devel()
devtools::check_win_release()


# check urls
urlchecker::url_check()


# release
# devtools::release() deprecated
# get tar.gz
devtools::build()
# check finished tarball
devtools::check_built("../unisono_0.1.0.tar.gz", cran = TRUE)


# 5. Build documentation website (optional)
# pkgdown::build_site()
