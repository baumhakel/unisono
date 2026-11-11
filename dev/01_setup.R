# ==============================================================================
# dev/01_setup.R  -  run once during package setup (not part of the build)
# ==============================================================================
#
# unisono: uniformity-based test for isotropy of non-scalar random fields
#
# Every call here is idempotent-ish: re-running only re-writes / re-adds
# entries, but there is normally no reason to run this file again.
# Per-release checks live in dev/02_release.R.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. License & package documentation
# ------------------------------------------------------------------------------
usethis::use_mit_license()
usethis::use_package_doc() # creates R/unisono-package.R

devtools::document()

# ------------------------------------------------------------------------------
# 2. C++ integration
# ------------------------------------------------------------------------------
usethis::use_rcpp() # src/, LinkingTo + Imports: Rcpp, .gitignore
# use_rcpp() only *prints* the roxygen tags -- add them manually to
# R/unisono-package.R:
#
#   #' @useDynLib unisono, .registration = TRUE
#   #' @importFrom Rcpp sourceCpp
#   NULL
#
# then run devtools::document()

# ------------------------------------------------------------------------------
# 3. Dependencies (modifies DESCRIPTION)
# ------------------------------------------------------------------------------
# Call imported functions as pkg::fun() throughout R/.
# Every package listed here must actually be used, otherwise R CMD check
# gives a NOTE ("Namespace in Imports field not imported from").

## Imports - computation
usethis::use_package("CompQuadForm") # Davies method (weighted chi-square quantiles)
usethis::use_package("mvtnorm") # qmvnorm
usethis::use_package("cli") # messages / errors

## Imports - data handling (roseplot)
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("purrr")
usethis::use_package("tibble") # check: still needed?
usethis::use_package("rlang") # .data pronoun

## Imports - plotting
usethis::use_package("ggplot2")
usethis::use_package("ggrepel") # roseplot labels

## Suggests - must be used conditionally in examples / vignettes:
##   if (requireNamespace("geoR", quietly = TRUE)) { ... }
usethis::use_package("geoR", type = "Suggests")
usethis::use_package("sp", type = "Suggests") # SpatialGridDataFrame
usethis::use_package("withr", type = "Suggests") # tests

## Pipe: use native |> and require R >= 4.1 (no magrittr needed)
usethis::use_package("R", type = "Depends", min_version = "4.1.0")


# ------------------------------------------------------------------------------
# 4. Testing
# ------------------------------------------------------------------------------
usethis::use_testthat(3)
# create test files as needed, e.g.:
# usethis::use_test("uni_test")

# ------------------------------------------------------------------------------
# 5. Vignette
# ------------------------------------------------------------------------------
# usethis::use_vignette("methodology-overview")  # adds knitr/rmarkdown to Suggests
# not added for now

# ------------------------------------------------------------------------------
# 6. GitHub
# ------------------------------------------------------------------------------
usethis::use_github_links() # URL + BugReports in DESCRIPTION


# ------------------------------------------------------------------------------
# 7. Files excluded from the build (.Rbuildignore)
# ------------------------------------------------------------------------------
usethis::use_build_ignore(c(
  "dev", # these setup / release scripts
  "renv", # renv library (note: no leading dot)
  "renv.lock",
  ".github", # GitHub Actions (incl. R-hub workflow)
  "whoami"
))
# check result with: readLines(".Rbuildignore")

# ------------------------------------------------------------------------------
# 8. CRAN release infrastructure
# ------------------------------------------------------------------------------
usethis::use_cran_comments() # cran-comments.md (auto-added to .Rbuildignore)
usethis::use_news_md() # NEWS.md (shipped; shown as changelog on CRAN)

# R-hub v2: runs checks via GitHub Actions in this repo.
# Needs a GitHub token once:
#   usethis::create_github_token()
#   gitcreds::gitcreds_set()
rhub::rhub_setup() # writes .github/workflows/rhub.yaml
# -> commit + push the workflow file, then:
rhub::rhub_doctor() # verifies token + repo setup

# ==============================================================================
# TODO
# ==============================================================================
# - [ ] Remove unused Imports (search R/ for "pkg::" for each package)
# - [ ] Replace magrittr `.` in geom_text_repel() with explicit data / .data$col
