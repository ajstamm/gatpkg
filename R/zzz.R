# Package structure functions ####

# settings when package is loaded (by calling library())
# or by calling devtools::load_all?
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-gat",
    devtools.install.args = "",
    devtools.name = "NYS GAT Package",
    devtools.desc.author = "Abigail Stamm <abigail.stamm@health.ny.gov> [aut, cre]",
    devtools.desc.license = "What license is it under?",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}

# settings when package is unloaded (needs to be defined)
# .onUnload()

# to create hftown ####
hftown <- rgdal::readOGR(
  dsn = "inst/extdata",
  layer = "hftown",
  stringsAsFactors = FALSE
)
hftown@data <- foreign::read.dbf(
  "inst/extdata/hftown.dbf",
  as.is = TRUE # maintains numerics
)
hftown <- sp::spTransform(hftown, sp::CRS("+proj=longlat +datum=NAD27"))

# to create base population for weighting ####
# not actually used

hfpop <- importGATweights(
  area = hftown,
  filein = "hfblock",
  pathin = "inst/extdata",
  popvar = "Pop_tot"
)

# to create hfagg5k ####

gatvars <- list(myidvar = "ID", aggregator1 = "TOTAL_POP",
                aggregator2 = "TOTAL_POP", minvalue1 = 5000,
                minvalue2 = 5000, boundary = "COUNTY",
                rigidbound = TRUE, popwt = FALSE)
mergevars <- list(mergeopt1 = "similar", similar1 = "B_TOT",
                  similar2 = "W_TOT")
ratevars <- list(ratename = "no_rate") # not calculated
aggvars <- defineGATmerge(area = hftown, gatvars = gatvars,
                          mergevars = mergevars)

hfagg5k <- mergeGATareas(ratevars = ratevars, aggvars = aggvars,
                         idvar = gatvars$myidvar, myshp = hftown)
vars <- c("ID", "NAME_SHORT", "COUNTY", "AREALAND", "AREAWATR", "TOTAL_POP",
          "W_TOT", "B_TOT", "HISP", "flag", "GATx", "GATy", "num_IDs")
hfagg5k@data <- hfagg5k@data[, names(hfagg5k@data)[names(hfagg5k@data) %in% vars]]

# to create hfcrosswalk ####
# currently, hfcrosswalk is not exported
# hfcrosswalk <- hftown
# hfcrosswalk@data <- cbind(hftown@data, data.frame(GATid = aggvars$IDlist))



