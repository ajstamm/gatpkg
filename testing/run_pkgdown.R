# recompile package
devtools::check()
devtools::build()
# devtools::install() doesn't work for some reason

# uninstall gatpkg, then restart session
# compile from zip
ver <- "gatpkg_2.1.0.tar.gz"
path <- paste("C:/Users/ajstamm/Documents/GitHub/Coding/R repos",
              ver, sep = "/")
install.packages(path, repos = NULL, type = "source")

# to build offline, then push to github
options(pkgdown.internet = FALSE)

# create site with pkgdown
pkgdown::build_site(install = FALSE, new_process = FALSE)

# if images are glitching, may need to run "find in files"
# and search for "", then replace all with ""

# find in files "Gwen Babcock, Abigail Stamm, Centers"
# and replace with "Gwen Babcock and Abigail Stamm with funding from Centers"

