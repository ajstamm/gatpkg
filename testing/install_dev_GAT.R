# install devtools if you don't already have it
install.packages("devtools")
# install the development version of GAT from Github with all 
# required packages from CRAN
devtools::install_github("ajstamm/gatpkg@Dev", dependencies = TRUE, 
                         build_vignette = TRUE)

# load GAT
library(gatpkg)
# run program
runGATprogram()


