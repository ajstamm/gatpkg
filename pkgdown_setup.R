# set up website

# Run once to configure package to use pkgdown
# usethis::use_pkgdown()

# deactivate internet check - will sync later
options(pkgdown.internet = F)

# Run to build the website
pkgdown::build_site(new_process = FALSE, devel = TRUE)

# after everything is working, rerun
pkgdown::build_site(new_process = TRUE, devel = TRUE)

# fix reference list
pkgdown::build_reference_index()

# documentation failing:
#
# issue appears to be in reading figures
# need to rerun code to create function help files

# building the package
devtools::document() # create help files
devtools::check()
devtools::build()

