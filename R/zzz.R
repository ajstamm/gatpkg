# Package structure functions ----

# settings when package is loaded (by calling library())
# or by calling devtools::load_all?
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-gat",
    devtools.install.args = "",
    devtools.name = "NYS GAT Package",
    devtools.desc.author = "Abigail Stamm <abigail.stamm@health.ny.gov> [aut, cre]",
    devtools.desc.license = "MIT License",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}

# settings when package is unloaded (needs to be defined)
# .onUnload()

# to create hftown ----
hftown <- sf::st_read(dsn = "inst/extdata", layer = "hftown")
hftown <- sf::st_transform(hftown, "+proj=longlat +datum=NAD27")
# sf::st_crs(hftown)

# to create base population for weighting ----
# not actually used

hfpop <- importGATweights(
  area = hftown,
  filein = "hfblock",
  pathin = "inst/extdata",
  popvar = "Pop_tot"
)

# to create aggregations ----
mergevars <- list(
  mergeopt1 = "closest",    # can be similar, closest, or least
  similar1 = "AREAWATR",    # numeric variable
  similar2 = "AREALAND",    # numeric variable without any zeros
  centroid = "geographic"
)

ratevars <- list(ratename = "no_rate") # not calculated

exclist <- list(var1 = "TOTAL_POP",
                math1 = "less than", # or greater than, equals
                val1 = 200,
                var2 = "NONE",
                var3 = "NONE")

  # settings for first aggregation ----
gatvars <- list(
  myidvar = "ID",             # character variable of unique values
  aggregator1 = "TOTAL_POP",  # numeric variable
  aggregator2 = "TOTAL_POP",  # numeric variable
  minvalue1 = 6000, minvalue2 = 6000,
  maxvalue1 = 10000, maxvalue2 = 10000,
  boundary = "COUNTY",        # character variable of non-unique values
  rigidbound = TRUE,          # boolean to enforce boundary
  popwt = TRUE,               # boolean for population weighting
  popvar = "Pop_tot"          # aggregation variable in population laayer
)

exclist <- list(
  var1 = "TOTAL_POP", math1 = "equals", val1 = 0,
  var2 = "NONE", # if not "NONE", define math2 & val2
  var3 = "NONE"  # if not "NONE", define math3 & val3
)

filevars <- list(
  popfile = "hfblock",
  poppath = paste0(find.package("gatpkg"), "/extdata")
)

  # first aggregation ----
aggvars <- defineGATmerge(area = hftown, gatvars = gatvars,
                          filevars = filevars, mergevars = mergevars,
                          exclist = exclist, progressbar = FALSE)


  # not fixed below this point ----
hfagg610k <- mergeGATareas(ratevars = ratevars, aggvars = aggvars,
                         idvar = gatvars$myidvar, myshp = hftown)

vars <- c("ID", "TOWN", "COUNTY", "TOTAL_POP", "GATflag", "GATx", "GATy",
          "GATnumIDs")

hfagg610k@data <- hfagg610k@data[, names(hfagg610k@data)
                             [names(hfagg610k@data) %in% vars]]

  # to create hfcrosswalk ----
hfcw610k <- hftown
hfcw610k@data <- cbind(hftown@data, data.frame(GATid = aggvars$IDlist))

vars <- c("ID", "TOWN", "COUNTY", "TOTAL_POP", "GATflag", "GATid")

hfcw610k@data <- hfcw610k@data[, names(hfcw610k@data)
                                 [names(hfcw610k@data) %in% vars]]

  # settings for second aggregation ----
gatvars <- list(
  myidvar = "ID",             # character variable of unique values
  aggregator1 = "TOTAL_POP",  # numeric variable
  aggregator2 = "TOTAL_POP",  # numeric variable
  minvalue1 = 6000, minvalue2 = 6000,
  maxvalue1 = 15000, maxvalue2 = 15000,
  boundary = "COUNTY",        # character variable of non-unique values
  rigidbound = FALSE,          # boolean to enforce boundary
  popwt = FALSE,              # boolean for population weighting
  popvar = "Pop_tot"          # aggregation variable in population laayer
)

temp <-
  ifelse(data.frame(hfagg610k[, gatvars$aggregator1]) > gatvars$maxvalue1 |
           data.frame(hfagg610k[, gatvars$aggregator2]) > gatvars$maxvalue2, 5, 0)
colnames(temp) <- NULL
hfagg610k$GATflag <- temp
rm(temp)

  # second aggregation ----
aggvars <- defineGATmerge(area = hfagg610k, gatvars = gatvars,
                          mergevars = mergevars,
                          progressbar = FALSE)

hfagg615k <- mergeGATareas(ratevars = ratevars, aggvars = aggvars,
                         idvar = gatvars$myidvar, myshp = hfagg610k)

vars <- c("ID", "TOWN", "COUNTY", "TOTAL_POP", "GATflag", "GATx", "GATy",
          "GATnumIDs")

hfagg615k@data <- hfagg615k@data[, names(hfagg615k@data)
                             [names(hfagg615k@data) %in% vars]]

  # to create hfcrosswalk ----
hfcw615k <- hfagg610k
hfcw615k@data <- cbind(hfcw615k@data, data.frame(GATid = aggvars$IDlist))

vars <- c("ID", "TOWN", "COUNTY", "TOTAL_POP", "GATflag", "GATid")

hfcw615k@data <- hfcw615k@data[, names(hfcw615k@data)
                                 [names(hfcw615k@data) %in% vars]]


# ----

