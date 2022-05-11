[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN status](https://www.r-pkg.org/badges/version/sword)](https://CRAN.R-project.org/package=sword)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)


**Please note: If you update R to 4.2.0, there appears to be a conflict with the package tcltk2. If you install the master version of GAT on R 4.2.0, GAT may break.**


# Geographic Aggregation Tool (GAT)


The geographic aggregation tool (GAT) was developed by the New York State Department of Health (NYSDOH) Environmental Public Health Tracking (EPHT) Program with funding from the CDC administered through Health Research Incorporated. GAT is maintained by Abigail Stamm at the NYSDOH. 

GAT aggregates, or dissolves, geographic areas based on numeric values for each area, such as case or population numbers.

View [GAT's webpage](https://ajstamm.github.io/gatpkg/docs/dev/index.html).

### Installing GAT 

Run the code below in R to install GAT directly from GitHub.

``` r
# install devtools if you don't already have it
install.packages("devtools")
# install the development version of GAT from Github with all 
# required packages from CRAN
devtools::install_github("ajstamm/gatpkg@Dev", dependencies = TRUE)
```

If running the code above doesn't work, you can also install GAT manually. Download the [most recently compiled version of GAT](compiles/gatpkg_1.61.0.tar.gz?raw=TRUE) or check the [compiles](compiles) folder for earlier versions of GAT, then download and follow [instructions on how to install GAT](presentations/gat_install_instructions.pdf), including a list of required R packages on CRAN.


### Why create GAT

Health outcome maps with fine geographic resolution can be misleading due to random fluctuations in disease rates caused by small numbers. In some cases these maps can also inadvertently disclose confidential data. To overcome these limitations we developed GAT to join neighboring geographic areas together until a user defined population and/or number of cases is reached. GAT can be used to produce maps for the public at the finest geographic resolution practicable.

### How GAT works

GAT reads in a polygon shapefile. The shapefile must contain at minimum a character variable that uniquely identifies areas and a numeric variable to sum for aggregation. A series of dialog boxes allows the user to select: 

1. a variable to uniquely identify areas
2. one or two aggregation variables
3. optionally, a variable of areas within which merging will be preferred (ex. county)
4. the value (sum) to which the selected aggregation variable(s) should be aggregated
5. the preferred aggregation method: closest geographic or population-weighted centroid, least value, or ratio of two values

GAT outputs two shapefiles, one containing aggregated areas and one containing a crosswalk between the original areas and the aggregated areas. GAT also outputs a log, several maps to help you identify potential issues in the aggregation process, and, optionally, a KML file.

The package for GAT includes a tutorial to learn the tool, an embedded map file to use when testing several of the package functions, and several other documents describing how GAT works and how to evaluate the resulting aggregated shapefile.

To learn more about GAT, view our [presentations](presentations/).


### Disclaimer

GAT is provided as is. We welcome feedback on what worked well, suggestions for improvement, and bugs you encounter. Report all issues via the "Issues" tab or by emailing [NYSDOH EPHT](mailto:epht@health.ny.gov?subject=[GAT in R]).

GAT was written in R-2.9.2 in Windows XP and was revised and converted to a package in R-3.4.3 in Windows 10 using RStudio-1.4.1103 and devtools-2.3.2. The latest version of GAT was compiled in R-3.6.1 and runs in R-3.5 through R-4.1. 

If you are interested in GAT's history, including the original SAS and R scripts, visit the [archive](archive/). 


