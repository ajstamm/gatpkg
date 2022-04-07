#' Write GAT Log
#'
#' This function writes a log of the aggregation process. It reports the
#' input and output datasets, variables and settings used, distributions of
#' aggregation variables, map projection, program start and end times, and
#' any warnings that were generated.
#'
#' @param area         A spatial polygons data frame.
#' @param gatvars      A list of objects created by the GAT tool. It contains
#'                     the strings myidvar, aggregator1, aggregator2,
#'                     mergeopt1,and boundary, which are all variables in the
#'                     area, the numbers minvalue1 and minvalue2, and the
#'                     boolean rigidbound. Both aggregator1 and aggregator2
#'                     must be numeric and myidvar must contain unique values.
#' @param aggvars      A list of objects created by the aggregation process.
#'                     See mergeGATpolygons() for the elements created.
#' @param filevars     A list of file names and paths. Of relevance to this
#'                     function are the filename, filein, and the combined save
#'                     path and save name, userout.
#' @param mysettings   A list of system settings, including version, pkgdate,
#'                     starttime, and the booleans savekml and exists.
#' @param mergevars    A list of settings for the aggregation, including type
#'                     of aggregation (mergeopt1) and, if relevant, the
#'                     variables to compare, similar1 and similar2.
#' @param ratevars     A list of settings for calculating rate, including
#'                     ratename, numerator and denominator variable names,
#'                     multiplier value, and color name and scheme for the map.
#' @param exclist      A list of exclusion criteria to use when merging.
#' @param settingsfile An R data file (*.Rdata) produced as part of GAT's
#'                     output. This file saves all settings for GAT and can be
#'                     used to reproduce the log. Other options can only be set
#'                     to NULL if this option is defined.
#'
#'
#' Notes on using the settingsfile option:
#'
#' 1. You will get an error if you moved the input shapefile before running
#'    the function with this option, since the function needs access to the
#'    input shapefile to recreate the log.
#' 2. Reading in an *.Rdata file from a previous version of GAT may result in
#'    incorrect elapsed time and GAT version numbers being written to the log
#'    due to changes in settings saved to the *.Rdata file as GAT has evolved.
#'
#' @examples
#'
#' # if you run this example, it saves "my_hftown.log" to your working
#' # directory
#'
#' if (interactive()) {
#' gatvars <- list(
#'   myidvar = "ID",             # character variable of unique values
#'   aggregator1 = "TOTAL_POP",  # numeric variable
#'   aggregator2 = "TOTAL_POP",  # numeric variable
#'   minvalue1 = 5000, minvalue2 = 5000,
#'   maxvalue1 = 16423, maxvalue2 = 15000,
#'   boundary = "COUNTY",        # character variable of non-unique values
#'   mergeopt1 = "closest",      # method used to merge polygons
#'   rigidbound = FALSE,         # boolean: were boundaries enforced?,
#'   savekml = FALSE,
#'   popwt = FALSE,
#'   exclmaxval = 2,
#'   ismax1 = TRUE,       # user selected "NONE" as maximum value
#'   ismin2 = FALSE,
#'   ismax2 = FALSE
#' )
#'
#' mergevars <- list(
#'   mergeopt1 = "similar",    # can be similar, closest, or least
#'   similar1 = "AREAWATR",    # numeric variable
#'   similar2 = "AREALAND",    # numeric variable without any zeros
#'   centroid = "geographic"
#' )
#'
#' ratevars <- list(
#'   ratename = "pop_density",
#'   numerator = "TOTAL_POP",
#'   denominator = "AREALAND",
#'   multiplier = 5000,
#'   colorname = "Blue-Green",
#'   colorscheme = "BuGn"
#' )
#'
#' aggvars <- defineGATmerge(
#'   area = hftown,
#'   gatvars = gatvars,
#'   mergevars = mergevars
#' )
#'
#' filevars <- list(
#'   filein = "hftown",                        # original filename
#'   userin = paste0(getwd(), "/hftown"),      # original file and path name
#'   userout = paste0(getwd(), "/my_hftown"),  # save file path and name
#'   pathout = getwd(),                        # save path name
#'   fileout = "my_hftown"                     # save file name
#' )
#'
#' mysettings <- list(
#'   starttime = Sys.time(),
#'   version = "1.0",
#'   pkgdate = format(Sys.Date(), "%m-%d-%Y"),
#'   adjacent = TRUE,
#'   pwrepeat = FALSE,
#'   minfirst = TRUE,
#'   exists = FALSE
#' )
#'
#' exclist <-
#'   list(
#'     var1 = "exclusion1",
#'     var2 = "exclusion2",
#'     var3 = "NONE",           # flag to denote no third variable
#'     math1 = "greater than",
#'     math2 = "less than",
#'     math3 = "equals",
#'     val1 = 5000,
#'     val2 = 50,
#'     val3 = 0,
#'     flagsum = 5
#'   )
#'
#' writeGATlog(
#'   area = hftown,
#'   gatvars = gatvars,
#'   filevars = filevars,
#'   aggvars = aggvars,
#'   mysettings = mysettings,
#'   mergevars = mergevars,
#'   ratevars = ratevars,
#'   exclist = exclist
#' )
#' }
#' @export

# should the log include these?
# * rate calculations
# * GAT version? currently reads in package version
# * gatpkg citation?
# re-order distributions by aggregation variable? - done

writeGATlog <- function(area = NULL, gatvars = NULL, aggvars = NULL,
                        filevars = NULL, mysettings = NULL,
                        mergevars = NULL, ratevars = NULL,
                        exclist = NULL, settingsfile = NULL) {
  # set up ####
  if (!is.null(settingsfile)) {
    load(settingsfile)
    if (is.null(mysettings)) { # rerunning failed log
      mysettings <- list(version = packageDescription("gatpkg")$Version,
                         pkgdate = packageDescription("gatpkg")$Date,
                         adjacent = "unknown",
                         pwrepeat = "unknown",
                         minfirst = "unknown",
                         limitdenom = "unknown",
                         starttime = Sys.time()) # needed for the log
    }
    mysettings$exists = file.exists(paste0(filevars$userout, ".shp"))
    area <- sf::st_read(dsn = filevars$pathin,
                        layer = filevars$filein)
  }

  # fill in full list of names below; code will error otherwise
  listitems <- names(area)
  listitems <- listitems[listitems != "GATflag"]
  myvars <- ""
  for (i in 1:(length(listitems)-1)) {
    myvars <- paste0(myvars, listitems[i], ", ")
    if (i %% 6 == 0) {
      myvars <- paste0(myvars, "\n", paste(rep(" ", 22), collapse = ""))
    }
  }
  myvars <- paste0(myvars, listitems[length(listitems)])
  if (gatvars$aggregator2 == "NONE") {
    gatvars$aggregator2 <- gatvars$aggregator1
  }

  # begin log file ####
  endtime <- Sys.time()
  logfile <- paste0(filevars$userout, ".log")

  # GAT settings ####
  logtext <- c("NYSDOH Geographic Aggregation Tool (GAT) Log",
               "\n  Version & date:", mysettings$version, mysettings$pkgdate,
               "\n  Date run:", as.character(Sys.Date()),
               "\n  Time GAT took to run:",
               round(difftime(endtime, mysettings$starttime, units = "mins"),
                     digits = 2), "minutes", "\n")
  write(logtext, file = logfile, ncolumns = length(logtext), append = FALSE)

  # input file ####
  logtext <- c("\nInput file:          ", filevars$userin,
               "\n  Projection:        ",
               sf::st_crs(area, parameters = TRUE)$proj4string,
               "\n  Field names:       ", myvars,
               "\n  Identifier:        ", gatvars$myidvar,
               "\n  Adjacency required?", mysettings$adjacent,
               "\n  Boundary variable: ", gatvars$boundary)
  if (!gatvars$rigidbound & gatvars$boundary != "NONE") {
    logtext <- c(logtext, "\n    You did not require the aggregation to",
                 "respect the borders of", gatvars$boundary)
  } else if (gatvars$boundary != "NONE") {
    logtext <- c(logtext, "\n    You chose to require the aggregation to",
                 "respect the borders of", gatvars$boundary)
  }
  write(logtext, file = logfile, ncolumns = length(logtext), append = TRUE)

  # Output file ####
  logtext <- c("\nOutput file:", filevars$userout,
               "\n  Number of input areas:    ",
               format(gatvars$numrow, big.mark=",", scientific=FALSE),
               "\n  Number of output areas:   ",
               format(nrow(aggvars$allpolydata), big.mark=",",
                      scientific=FALSE),
               # does not take into account aborted aggregations
               "\n  Number of aggregations:   ",
               format(nrow(area) - nrow(aggvars$allpolydata),
                      big.mark=",",
                      scientific=FALSE),
               "\n  Number of excluded areas: ",
               format(exclist$flagsum, big.mark=",", scientific=FALSE))
  write(logtext, file = logfile, ncolumns = length(logtext), append = TRUE)

  # Merge settings ####
  logtext <- c("\nMerge type:", mergevars$mergeopt1,
               "\n  Prefer aggregating to areas below minimum value first?",
               mysettings$minfirst)
  if (mergevars$mergeopt1 == "similar") {
    logtext <- c(logtext, "\n  First similar variable:  ", mergevars$similar1,
                 "\n  Second similar variable: ", mergevars$similar2)
  } else if (mergevars$mergeopt1 == "closest") {
    logtext <- c(logtext, mergevars$centroid, "centroid")
    if (mergevars$centroid == "population-weighted") {
      logtext <- c(logtext, "\n  Population file:", filevars$popin,
                   "\n  Population variable:", gatvars$popvar,
                   "\n  Recalculate centroid after each merge?",
                   mysettings$pwrepeat)
    }
  }
  write(logtext, file = logfile, ncolumns = length(logtext), append = TRUE)

  # Exclusion criteria ####
  if (exclist$var1 != "NONE" | exclist$var1 != "NONE" | exclist$var1 != "NONE") {
    logtext <- c("\nExclusion criteria:")
    if (exclist$var1 != "NONE") {
      logtext <- c(logtext, "\n  1. ", exclist$var1, exclist$math1,
                   format(exclist$val1, big.mark=",", scientific=FALSE))
    }
    if (exclist$var2 != "NONE") {
      logtext <- c(logtext, "\n  2. ", exclist$var2, exclist$math2,
                   format(exclist$val2, big.mark=",", scientific=FALSE))
    }
    if (exclist$var3 != "NONE") {
      logtext <- c(logtext, "\n  3. ", exclist$var3, exclist$math3,
                   format(exclist$val3, big.mark=",", scientific=FALSE))
    }
    write(logtext, file = logfile, ncolumns = length(logtext), append = TRUE)
  }

  # First aggregation variable ####
  min1 <- format(gatvars$minvalue1, big.mark=",", scientific=FALSE)
  max1 <- format(gatvars$maxvalue1, big.mark=",", scientific=FALSE)
  if (!is.null(gatvars$ismax1)) {
    if (gatvars$ismax1) max1 <- paste(max1, "(no maximum)")
  }

  logtext <- c("\nFirst aggregation variable:", gatvars$aggregator1,
               "\n  Minimum value:", min1, "\n  Maximum value:", max1,
               "\nPre-aggregation distribution:")
  write(logtext, file = logfile, ncolumns = length(logtext), append = TRUE)
  write.table(quantile(data.frame(area)[, gatvars$aggregator1]), file = logfile,
              row.names = TRUE, col.names = FALSE, append = TRUE)

  logtext <- c("\nPost-aggregation distribution:")
  write(logtext, file = logfile, ncolumns = length(logtext), append = TRUE)

  write.table(quantile(data.frame(aggvars$allpolydata)[, gatvars$aggregator1]),
              file = logfile, row.names = TRUE, col.names = FALSE,
              append = TRUE)

  # second aggregation variable ####
  if (gatvars$aggregator1 != gatvars$aggregator2) {
    min2 <- format(gatvars$minvalue2, big.mark=",", scientific=FALSE)
    if (!is.null(gatvars$ismin2)) {
      if (gatvars$ismin2) min2 <- paste(min2, "(no minimum)")
    }
    max2 <- format(gatvars$maxvalue2, big.mark=",", scientific=FALSE)
    if (!is.null(gatvars$ismax2)) {
      if (gatvars$ismax2) max2 <- paste(max2, "(no maximum)")
    }

    logtext <- c("\n\nSecond aggregation variable:", gatvars$aggregator2,
                 "\n  Minimum value:", min2, "\n  Maximum value:", max2,
                 "\nPre-aggregation distribution:")
    write(logtext, file = logfile, ncolumns = length(logtext), append = TRUE)
    write.table(quantile(data.frame(area)[, gatvars$aggregator2]), file = logfile,
                row.names = TRUE, col.names = FALSE, append = TRUE)
  }

  if (gatvars$aggregator1 != gatvars$aggregator2) {
    logtext <- c("\nPost-aggregation distribution:")
    write(logtext, file = logfile, ncolumns = length(logtext), append = TRUE)
    write.table(quantile(data.frame(aggvars$allpolydata)[, gatvars$aggregator2]),
                file = logfile, row.names = TRUE, col.names = FALSE,
                append = TRUE)
  }

  # rate calculation if requested ####
  if (ratevars$ratename == "no_rate") {
    logtext <- "\nYou chose not to calculate a rate.\n"
  } else {
    logtext <- paste0("\nGAT calculated the rate ", ratevars$ratename,
                      " using the color scheme ", ratevars$colorname, ".")
    logtext <- c(logtext, paste0("\n  Numerator:   ", ratevars$numerator),
                 paste0("\n  Denominator: ", ratevars$denominator),
                 paste0("\n  Multiplier:  ",
                        format(as.numeric(ratevars$multiplier), big.mark=",",
                                          scientific=FALSE)), "\n")
  }
  write(logtext, file = logfile, ncolumns = length(logtext), append = TRUE)

  # saved files ####
  logtext <- c("All files have been saved to ", filevars$pathout)
  if (!mysettings$exists) {
    logtext <- c(logtext, "\n  The shapefiles failed to save. ")
  } else {
    logtext <- c(logtext,
                 "\n  Aggregated shapefile:             ",
                 paste0(filevars$fileout, ".shp"),
                 "\n    Variables created by GAT:",
                 "\n        GATx:",
                 "longitude of the aggregated area", mergevars$centroid,
                 "centroid",
                 "\n        GATy:",
                 "latitude of the aggregated area", mergevars$centroid,
                 "centroid",
                 "\n        GATcratio:",
                 "compactness ratio, or the area of the polygon over the",
                 "area of a circle with the same perimeter",
                 "\n        GATnumIDs:",
                 "number of original areas that were merged into each",
                 "aggregated area",
                 "\n        GATflag:",
                 "flag of areas that were excluded from aggregation or",
                 "generated warnings in the log",
                 "\n            value = 0:", "no flag",
                 "\n            value = 1:",
                 "area excluded based on exclusion criteria",
                 "\n            value = 5:",
                 "area excluded because value of aggregation variable",
                 "exceeded maximum value",
                 "\n            value = 10:",
                 "value of area's aggregation variable is below minimum",
                 "value, but there are no eligible",
                 "\n                        areas for further aggregation")
    if (ratevars$ratename != "no_rate") {
      logtext <- c(logtext,
                   paste0("\n        ", ratevars$ratename, ":"),
                   "your rate, ratio, or density")
    }
    if (gatvars$popwt) {
      logtext <- c(logtext,
                   "\n        GATpop:",
                   "population of the aggregated area, from the population",
                   "file")
    }
    logtext <- c(logtext,
                 "\n  Original shapefile with crosswalk:",
                 paste0(filevars$fileout, "in.shp"),
                 "\n    Variables created by GAT:",
                 "\n        GATflag:",
                 "flag of areas that were excluded from aggregation or",
                 "generated warnings in the log",
                 "\n            value = 0:", "no flag",
                 "\n            value = 1:",
                 "area excluded based on exclusion criteria",
                 "\n            value = 5:",
                 "area excluded because value of aggregation variable",
                 "exceeded maximum value",
                 "\n        GATid:",
                 "GAT-generated identifier of the aggregated area each",
                 "original area fell inside when aggregated")
  }
  logtext <- c(logtext,
               "\n  Maps:                             ",
               paste0(filevars$fileout, "plots.pdf"),
               "\n  Log file:                         ",
               paste0(filevars$fileout, ".log"),
               "\n  R settings file:                  ",
               paste0(filevars$fileout, "settings.Rdata"))
  if (gatvars$savekml) {
    logtext <- c(logtext,
                 "\n  KML file:                         ",
                 paste0(filevars$fileout, ".kml"))
  } else {
    logtext <- c(logtext, "\n  You chose not to write a KML file.")
  }

  # warnings and errors ####
  if (aggvars$logmsg != "") logtext <-
    c(logtext, "\n\nThe following warnings were called while aggregating areas:",
      "\n", trimws(aggvars$logmsg))
  write(logtext, file = logfile, ncolumns = length(logtext), append = TRUE)
  #end code to create log file
}

