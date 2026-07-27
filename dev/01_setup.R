# ==============================================================================
# dev/01_setup.R - Run once during package setup
# ==============================================================================

# unisono
# uniformity-based test for isotropy for non-scalar objects


# 1. License & Description Infrastructure
usethis::use_mit_license()
usethis::use_package_doc()

# 2. Package Dependencies (Modifies DESCRIPTION automatically)

# usethis::use_package("data.table", type = "Imports") ?????
usethis::use_package("Rcpp", type = "Imports")

usethis::use_package("CompQuadForm", type = "Imports")
usethis::use_package("mvtnorm", type = "Imports")
usethis::use_package("tibble", type = "Imports")
usethis::use_package("dplyr", type = "Imports")
usethis::use_package("rlang", type = "Imports")
usethis::use_package("purrr", type = "Imports")
usethis::use_package("tidyr", type = "Imports")
usethis::use_package("ggplot2", type = "Imports")
usethis::use_package("ggrepel", type = "Imports")
usethis::use_package("cli", type = "Imports")




# suggest
usethis::use_package("geoR", type = "Suggests")
# usethis::use_package("vdiffr", type = "Suggests")   cool, but not working in our case
usethis::use_package("withr", type = "Suggests")
usethis::use_package("sp", type = "Suggests")


# ignore these directories
usethis::use_build_ignore(".renv")
usethis::use_build_ignore("rhub")
usethis::use_build_ignore("whoami")
usethis::use_build_ignore(".github")

# usethis::use_package("magrittr", type = "Imports") "(or rlang / dplyr for the . pronoun in geom_text_repel)"

# Packages previously used (check, whether actually needed, and what for)
# library(expm)
# library(CompQuadForm) # for davies method (quantiles of weighted chisq)
# library(geoR)
# library(ggplot2)
# library(tidyverse) # for roseplot data manipulation
# library(ggrepel) # for roseplot labels
# library(mvtnorm) # for qmvnorm
# sp for SpatialGridDataFrame

# todo: call them with :: explicitly.
# todo: remove all delta occurances
# do not stop when not all are same length: this might be on purpose to compensate delta


# 3. Testing Infrastructure
usethis::use_testthat(3)
# usethis::use_test("fit_model") ??

# 4. Vignettes & C++ Integration
usethis::use_vignette("methodology-overview")     # ???
usethis::use_rcpp() # does not work?


usethis::use_package_doc()


