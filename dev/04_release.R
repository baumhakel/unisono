# ==========================================================================
# 0. Setup
# ==========================================================================
usethis::use_release_issue()   # GitHub issue with the CRAN checklist
# first release: usethis::use_release_issue(version = "0.1.0")

# ==========================================================================
# 1. Docs and metadata
# ==========================================================================
devtools::document()
devtools::spell_check()                  # add false positives to inst/WORDLIST
urlchecker::url_check()
urlchecker::url_update()                 # auto-fixes redirects
tools::showNonASCIIfile(list.files("R", full.names = TRUE))  # e.g. "ö" in Hörmann -> \u00f6

for (f in list.files("R", full.names = TRUE)) {
  cat("==", f, "\n")
  tools::showNonASCIIfile(f)
}

# ==========================================================================
# 2. Tests and coverage
# ==========================================================================
devtools::test()                         # Ctrl+Shift+T
cover_res <- covr::package_coverage()
cover_res
covr::zero_coverage(cover_res)

# ==========================================================================
# 3. Examples (CRAN runs \donttest{} too)
# ==========================================================================
devtools::run_examples(run_donttest = TRUE)

# ==========================================================================
# 4. Optional code-quality pass
# ==========================================================================
goodpractice::gp()
lintr::lint_package()

# ==========================================================================
# 5. Local CRAN check
# ==========================================================================
devtools::check(cran = TRUE)             # Ctrl+Shift+E; aim for 0 errors/warnings/notes

# ==========================================================================
# 6. Remote checks (results arrive by email / GitHub Actions)
# ==========================================================================
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrel()             # only if Depends allows older R
devtools::check_mac_release()

rhub::rhub_check(platforms = c("linux", "macos-arm64", "windows",
                               "atlas", "mkl", "nosuggests"))
# compiled code? add "clang-asan", "valgrind"

# ==========================================================================
# 7. Build and check the final tarball
# ==========================================================================
tarball <- devtools::build()             # returns the path
devtools::check_built(tarball, cran = TRUE)

# ==========================================================================
# 8. Submit
# ==========================================================================
# - Update cran-comments.md: the environments from step 6, plus a short
#   explanation of any NOTE (a first submission always gets the
#   "New submission" NOTE; just mention it)
# - Bump the version if you changed anything after step 6
devtools::submit_cran()                  # or upload the tarball at cran.r-project.org/submit.html
# -> click the confirmation link in the email CRAN sends you
