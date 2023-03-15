# set up website
# https://pkgdown.r-lib.org/articles/pkgdown.html

# Run once to configure package to use pkgdown
# usethis::use_pkgdown()
# weird bug in pkgdown v2.0 where you need to install ragg first? didn't work
# reverted to pkgdown v1.6.1

# deactivate internet check - will sync later
options(pkgdown.internet = F)
options(rmarkdown.html_vignette.check_title = FALSE)

# after everything is working, rerun
pkgdown::build_site(new_process = TRUE, devel = FALSE)
# for some reason index is not updating correctly
# references to GATv2 not saving from readme
# needed to revise manually ...
#  lifecycle, build info, version & instructions links
# check my home account for an index.md

# Run to build the website
pkgdown::build_site(new_process = FALSE)

# fix reference list
pkgdown::build_reference_index()

# documentation failing:
# issue appears to be in reading figures
# need to rerun code to create function help files

# building the package
devtools::document() # create help files
devtools::check()
devtools::build()

# to debug, run ...
# from https://github.com/r-lib/downlit/issues/166
debug(grDevices::dev.new)
pkgdown::build_site(new_process = FALSE)


