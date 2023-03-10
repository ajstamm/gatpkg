# set up website

# Run once to configure package to use pkgdown
# usethis::use_pkgdown()
# weird bug in pkgdown v2.0 where you need to install ragg first? didn't work
# reverted to pkgdown v1.6.1

# deactivate internet check - will sync later
options(pkgdown.internet = F)

# after everything is working, rerun
pkgdown::build_site(new_process = TRUE, devel = TRUE)

# Run to build the website
pkgdown::build_site(new_process = FALSE, devel = TRUE)

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

