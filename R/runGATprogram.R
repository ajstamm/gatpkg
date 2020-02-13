#' Run GAT Program with one dialog for aggregators
#'
#' @description
#'
#' This function handles nearly the entire program, except for installing and
#' loading packages. It does not require any inputs, as the popup dialogs it
#' creates guide the user to define all parameters. For step by step details
#' on how this program works, browse the package vignettes, especially the
#' GAT manual, which you can access via
#' \href{../doc/gat_step_by_step.html}{
#' \code{vignette("gat_step_by_step", package = "gatpkg")}}.
#'
#' @param limitdenom Boolean denoting whether to force denominators in rates
#'                   and merge ratios to contain only non-zero values.
#' @param pwrepeat   A boolean denoting whether population weighting (if used)
#'                   should be recalculated each time two areas are merged
#'                   (TRUE) or if area centroids should be weighted with area
#'                   populations (FALSE). If population weighting is not used,
#'                   this option is ignored. If population weighting is used,
#'                   a polygon shapefile containing population is needed.
#' @param settings   The filepath to an Rdata file previously created by GAT.
#' @param adjacent   A boolean denoting whether to force GAT to merge only
#'                   adjacent areas.
#'
#' @details
#' You can load "BWidget" in advance, which opens a more navigable shapefile
#' selection window, but it is not necessary to run the function.
#'
#' @examples
#'
#' \donttest{
#' # this code will run the default version of GAT
#' runGATprogram() # uses defaults
#' }
#'
#' @export

# this will also incorporate max values

runGATprogram <- function(limitdenom = FALSE, pwrepeat = FALSE,
                          settings = NULL, adjacent = TRUE) {
  #  1. start the GAT program ####
  # load the progress bar
  mysettings <- list(version = packageDescription("gatpkg")$Version,
                     pkgdate = packageDescription("gatpkg")$Date,
                     starttime = Sys.time(), # needed for the log
                     quit = FALSE)

  pb <- list(title = paste("NYSDOH Geographic Aggregation Tool (GAT)",
                           mysettings$version, mysettings$date),
             label = "GAT is running. Please wait for dialogs.")
  tpb <- tcltk::tkProgressBar(title = pb$title, label = pb$label, min = 0,
                              max = 26, initial = 0, width = 400)

  # pre-load lists
  step <- 1 # start at step 1
  temp <- list(flagconfirm = FALSE, msg = "")
  myshps <- list()

  if (!is.null(settings)) {
    load(settings)
    step <- 11
    temp$flagconfirm <- TRUE
    filevars$userout <- paste0(filevars$userout, "_2")
    filevars$fileout <- paste0(filevars$fileout, "_2")
    temp$shp <- rgdal::readOGR(dsn = filevars$pathin,
                               layer = filevars$filein,
                               stringsAsFactors = FALSE)
    temp$mapdata <- foreign::read.dbf(paste0(filevars$userin, ".dbf"),
                                      as.is = TRUE)
    temp$mapflag <- temp$mapdata[temp$mapdata$GATflag == 0, ]
    temp$numerics <- checkGATvariabletypes(temp$mapdata, type = "number")
    temp$old_vars <- c()
    for (i in 1:ncol(temp$mapdata)) {
      # possibly these can be removed entirely, but need to verify
      # if removing, inform user with warning dialog
      # not sure how to handle flag variable - rename it?
      if (names(temp$mapdata)[i] %in%
          c("old_GATx", "old_GATy", "old_GATnumIDs", "old_GATcratio",
            "old_flag", "old_GATpop")) {
        temp$old_vars <- c(temp$old_vars, names(temp$mapdata)[i])
      }
    }
    if (length(temp$old_vars) > 0) {
      temp$msg <- paste(temp$msg,
                        paste(temp$old_vars, collapse = ", "),
                        "were removed from the data frame.")
    }
    temp$mapdata <- temp$mapdata[,
                                 names(temp$mapdata)[!names(temp$mapdata) %in%
                                                       temp$old_vars]]
    for (i in 1:ncol(temp$mapdata)) {
      if (names(temp$mapdata)[i] %in%
          c("GATx", "GATy", "GATnumIDs", "GATcratio", "GATflag", "GATpop")) {
        temp$msg <- paste(temp$msg,
                          "\n", names(temp$mapdata)[i], "has been changed to",
                          paste0("old_", names(temp$mapdata)[i], "."))
        names(temp$mapdata)[i] <- paste0("old_", names(temp$mapdata)[i])
      }
    }
    if (!temp$msg == "") {
      tcltk::tkmessageBox(title = "Some variable names changed", type = "ok",
                          icon = "warning", message = temp$msg)
    }

  } else {
    gatvars <- list()
    filevars <- list(userin = "")
    mergevars <- NULL
    ratevars <- NULL
    exclist <- NULL
    aggvars <- NULL
  }

  # pre-load number function
  numformat <- function(num) {
    format(as.numeric(gsub(",", "", num)), big.mark=",", scientific=FALSE)
  }


  #  2. start user input ####

  while (step < 20){ # get user input until settings confirmed
    while (step == 1) { # ask user to identify shapefile; check its usability
      pb <- list(title = "NYSDOH GAT: identify shapefile",
                 label = "Identifying and selecting the shapefile.")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                              label = pb$label)

      # identify shapefile
      filevars <- locateGATshapefile(myfile = filevars$userin, step = step)

      if (filevars$userin == "cancel") {
        step <- 20
        mysettings$quit <- TRUE
      } else {
        # read in map data only from the shapefile's dbf
        temp$mapdata <- foreign::read.dbf(paste0(filevars$userin, ".dbf"),
                                          as.is = TRUE)
        temp$shp <- rgdal::readOGR(dsn = filevars$pathin,
                                   layer = filevars$filein,
                                   stringsAsFactors = FALSE)

        # check for both numeric and character data
        temp$numerics <- checkGATvariabletypes(temp$mapdata, type = "number")
        temp$alphas <- checkGATvariabletypes(temp$mapdata, type = "character")
        temp$polys <- class(temp$shp)

        # add dialog specifying each issue
        # add check for shapefile type (if class(shp) )
        temp$msg <- ""
        temp$error <- FALSE
        if (nrow(temp$shp@data) < 2) {
          temp$issue <- "at least 2 areas."
          temp$error <- TRUE
        } else if (temp$polys != "SpatialPolygonsDataFrame") {
          # message: wrong kind of shapefile; repeat dialog
          temp$issue <- "polygons."
          temp$error <- TRUE
        } else if (length(temp$numerics) == 0) {
          # message: there are no numeric variables; repeat dialog
          temp$issue <- "any numeric variables."
          temp$error <- TRUE
        } else if (length(temp$alphas) == 0) {
          # message: there are no character variables; repeat dialog
          temp$issue <- "any non-numeric variables."
          temp$error <- TRUE
        } else {
          temp$old_vars <- c()
          for (i in 1:ncol(temp$mapdata)) {
            # possibly these can be removed entirely, but need to verify
            # if removing, inform user with warning dialog
            # not sure how to handle flag variable - rename it?
            if (names(temp$mapdata)[i] %in%
                c("old_GATx", "old_GATy", "old_GATnumIDs", "old_GATcratio",
                  "old_flag", "old_GATpop")) {
              temp$old_vars <- c(temp$old_vars, names(temp$mapdata)[i])
            }
          }
          if (length(temp$old_vars) > 0) {
            temp$msg <- paste(temp$msg,
                              paste(temp$old_vars, collapse = ", "),
                              "were removed from the data frame.")
          }
          temp$mapdata <- temp$mapdata[,
            names(temp$mapdata)[!names(temp$mapdata) %in% temp$old_vars]]
          for (i in 1:ncol(temp$mapdata)) {
            if (names(temp$mapdata)[i] %in%
                c("GATx", "GATy", "GATnumIDs", "GATcratio", "GATflag", "GATpop")) {
              temp$msg <- paste(temp$msg,
                                "\n", names(temp$mapdata)[i], "has been changed to",
                                paste0("old_", names(temp$mapdata)[i], "."))
              names(temp$mapdata)[i] <- paste0("old_", names(temp$mapdata)[i])
            }
          }
          if (temp$msg != "") {
            tcltk::tkmessageBox(title = "Some variable names changed", type = "ok",
                                icon = "warning", message = temp$msg)
          }
          step <- step + 1
        }
        if (temp$error) {
          temp$msg <- paste("The shapefile", filevars$filein,
                            "does not contain", temp$issue, "\n",
                            "Please select a new shapefile.")
          tcltk::tkmessageBox(title = "Shapefile invalid", type = "ok",
                              icon = "error", message = temp$msg)
          temp$error <- FALSE
        }
      }
    } # end request shapefile (userin)
    while (step == 2) { # ask user to select ID variable that uniquely
                        # identifies areas to be merged
      pb <- list(title = "NYSDOH GAT: identify identifier",
                 label = "Selecting the unique identifier.")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                              label = pb$label)

      # identify GAT polygon identifier variable
      gatvars$myidvar <- identifyGATid(mapdata = temp$mapdata, step = step)

      if (gatvars$myidvar == "back"){
        step <- step - 1
      } else if (gatvars$myidvar == "cancel") {
        mysettings$quit <- TRUE
        step <- 20
      } else if (gatvars$myidvar == "missing") {
        temp$msg <- paste("The shapefile does not contain a suitable ID variable.",
                          "A temporary ID will be created.")
        tcltk::tkmessageBox(title = "No suitable ID variable", message = temp$msg,
                            type = "ok", icon = "warning")
      } else { # go ahead
        if (temp$flagconfirm) {
          step <- 11
        } else {
          step <- step + 1
        }
      }
    } # end request id variable (myidvar)
    while (step == 3) { #ask user to select boundary variable, if present
      pb <- list(title = "NYSDOH GAT: identify boundary",
                 label = "Selecting the boundary variable.")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                              label = pb$label)
      if (is.null(gatvars$boundary)) {
        gatvars$boundary <- "NONE"
        gatvars$rigidbound <- FALSE
      }

      tempbound <- NULL

      while (is.null(tempbound)) {
        tempbound <- identifyGATboundary(data = temp$mapdata, step = step,
                                         boundary = gatvars$boundary,
                                         borders = gatvars$rigidbound)
      }

      gatvars$rigidbound <- tempbound$check
      gatvars$boundary <- tempbound$myvar


      if (gatvars$boundary == "NONE") {
        gatvars$rigidbound <- FALSE # just in case user selects both
      }

      if (gatvars$boundary == "back") {
        step <- step - 1
      } else if (gatvars$boundary == "cancel") {
        mysettings$quit <- TRUE
        step <- 20
      } else {
        if (temp$flagconfirm) {
          step <- 11
        } else {
          step <- step + 1
        }
      }
      rm(tempbound)
    } # end request boundary variable (boundaryvar)
    while (step == 4) { # ask for aggregation variables
      pb <- list(title = "NYSDOH GAT: identify aggregators",
                 label = "Selecting the aggregation variables.")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                              label = pb$label)

      if (is.null(gatvars$aggregator1)) {
        agglist <- NULL
      } else {
        agglist <- list(var1 = gatvars$aggregator1,
                        var2 = gatvars$aggregator2,
                        minval1 = gatvars$minvalue1,
                        maxval1 = gatvars$maxvalue1,
                        minval2 = gatvars$minvalue2,
                        maxval2 = gatvars$maxvalue2)
      }
      error <- TRUE

      # re-call the function as needed
      while (error) {
        agglist <- identifyGATaggregators(mapdata = temp$mapdata, step = step,
                                          agglist = agglist)
        error <- FALSE
        if (is.null(agglist)) {
          x <- confirmGATquit()
          if (x == "quit") {
            agglist <- list(var1 = "cancel")
          } else {
            error <- TRUE
            agglist <- list(var1 = "")

          }
        } else if (length(agglist$var1) == 0) {
          error <- TRUE
          agglist <- NULL
        }
      }

      if (agglist$var1 == "back") {
        step <- step - 1
      } else if (agglist$var1 == "cancel") {
        step <- 20
        mysettings$quit <- TRUE
      } else if (!error) {
        gatvars$aggregator1 <- agglist$var1
        gatvars$aggregator2 <- agglist$var2
        gatvars$minvalue1 <- agglist$minval1
        gatvars$maxvalue1 <- agglist$maxval1
        gatvars$minvalue2 <- agglist$minval2
        gatvars$maxvalue2 <- agglist$maxval2
        if (temp$flagconfirm) {
          step <- 11
        } else {
          step <- step + 1
        }
      }
      rm(agglist)
    } # end request aggregation variables (agglist)
    while (step == 5) {
      pb <- list(title = "NYSDOH GAT: Enter exclusions",
                 label = "Identifying your exclusion criteria.")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                              label = pb$label)

      if (!exists("exclist")) exclist <- NULL
      temp$error <- TRUE

      while (temp$error) {
        exclist <- inputGATexclusions(mapdata = temp$mapdata, step = step,
                                      exclist = exclist)
        temp$error <- FALSE
        if (is.null(exclist)) {
          x <- confirmGATquit()
          if (x == "quit") {
            exclist <- list(var1 = "cancel")
          } else {
            temp$error <- TRUE
            exclist <- list(var1 = "")
          }
        }

        # get rid of all non-numeric characters
        # move this to a function?
        if (!temp$error & !exclist$var1 %in% c("back", "cancel")) {
          if (exclist$math1 == "less than") {
            temp$minmaxlbl <- "maximum"
          } else if (exclist$math1 == "greater than") {
            temp$minmaxlbl <- "minimum"
          } else {
            temp$minmaxlbl <- ""
          }
          while (grepl("[^0-9.,-]", exclist$val1)) {
            gats <- list(title = paste("Threshold for", exclist$var1),
                         msg = paste("Please enter a valid", temp$minmaxlbl, "number for",
                                     exclist$var1),
                         help = paste0("To continue, enter a valid number and click 'Next >',",
                                       "\nto return to exclusion selection, click '< Back',",
                                       "\nand to quit the program, click 'Cancel'."))
            exclist$val1 <- inputGATvalue(title = gats$title, help = gats$help,
                                          message = gats$msg, defaulttext = "1,000",
                                          helppage = "inputGATvalue", step = step)
            if (exclist$val1 == "back") {
              exclist$var1 <- "back"
              exclist$val1 <- 0
            } else if (exclist$val1 == "cancel") {
              exclist$var1 <- "cancel"
              exclist$val1 <- 0
              mysettings$quit <- TRUE
              step <- 20
            }
          }
          while (grepl("[^0-9.,-]", exclist$val2)) {
            if (exclist$math2 == "less than") {
              temp$minmaxlbl <- "maximum"
            } else if (exclist$math2 == "greater than") {
              temp$minmaxlbl <- "minimum"
            } else {
              temp$minmaxlbl <- ""
            }
            gats <- list(title = paste("Threshold for", exclist$var2),
                         msg = paste("Please enter a valid", temp$minmaxlbl, "number for",
                                     exclist$var2),
                         help = paste0("To continue, enter a valid number and click 'Next >',",
                                       "\nto return to exclusion selection, click '< Back',",
                                       "\nand to quit the program, click 'Cancel'."))
            exclist$val2 <- inputGATvalue(title = gats$title, help = gats$help,
                                          message = gats$msg, defaulttext = "1,000",
                                          helppage = "inputGATvalue", step = step)
            if (exclist$val2 == "back") {
              exclist$var1 <- "back"
              exclist$val2 <- 0
            } else if (exclist$val2 == "cancel") {
              exclist$var1 <- "cancel"
              exclist$val2 <- 0
              mysettings$quit <- TRUE
              step <- 20
            }
          }
          while (grepl("[^0-9.,-]", exclist$val3)) {
            if (exclist$math3 == "less than") {
              temp$minmaxlbl <- "maximum"
            } else if (exclist$math3 == "greater than") {
              temp$minmaxlbl <- "minimum"
            } else {
              temp$minmaxlbl <- ""
            }
            gats <- list(title = paste("Threshold for", exclist$var3),
                         msg = paste("Please enter a valid", temp$minmaxlbl, "number for",
                                     exclist$var3),
                         help = paste0("To continue, enter a valid number and click 'Next >',",
                                       "\nto return to exclusion selection, click '< Back',",
                                       "\nand to quit the program, click 'Cancel'."))
            exclist$val3 <- inputGATvalue(title = gats$title, help = gats$help,
                                          message = gats$msg, defaulttext = "1,000",
                                          helppage = "inputGATvalue", step = step)
            if (exclist$val3 == "back") {
              exclist$var1 <- "back"
              exclist$val3 <- 0
            } else if (exclist$val3 == "cancel") {
              exclist$var1 <- "cancel"
              exclist$val3 <- 0
              mysettings$quit <- TRUE
              step <- 20
            }
          }

          exclist$val1 <- as.numeric(gsub(",", "", exclist$val1))
          exclist$val2 <- as.numeric(gsub(",", "", exclist$val2))
          exclist$val3 <- as.numeric(gsub(",", "", exclist$val3))
        } else if (exclist$var1 == "back") {
            step <- step - 1
        } else if (exclist$var1 == "cancel") {
            mysettings$quit <- TRUE
            step <- 20
        }
      }

      # calculate exclusions now to use them to calculate the
      # denominator for similar merges and rates
      temp$mapdata$GATflag <- 0
      temp$mapdata$GATflag <- calculateGATflag(exclist, temp$mapdata)
      exclist$flagsum <- sum(temp$mapdata$GATflag != 0)

      if (nrow(temp$mapdata) - exclist$flagsum < 2) {
        temp$msg <- paste("This selection will exclude", exclist$flagsum,
                          "of", nrow(temp$mapdata), "areas.", "\n",
                          "GAT requires at least 2 areas to run.", "\n",
                          "Please select new exclusion criteria.")
        tcltk::tkmessageBox(title = "Selections invalid", type = "ok",
                            icon = "error", message = temp$msg)
        temp$error <- TRUE
      } else if (exclist$flagsum > 0) {
        temp$msg <- "You have selected to exclude:"
        i <- list(var = c(exclist$var1, exclist$var2, exclist$var3),
                  math = c(exclist$math1, exclist$math2, exclist$math3),
                  val = c(exclist$val1, exclist$val2, exclist$val3))
        for (j in 1:3) {
          if (i$var[j] != "NONE") {
            temp$msg <- paste(temp$msg, "\n   ", i$var[j], i$math[j], i$val[j])
          }
        }

        temp$msg <- paste(temp$msg, "\nThis will exclude", exclist$flagsum,
                          "of", nrow(temp$mapdata), "areas.")
        temp$help <- paste0("To continue, select 'Yes',",
                            "\nto reselect exclusion criteria, select 'Repeat',",
                            "\nand to return to second aggregation variable selection,",
                            "click '< Back',")

        temp$cancel <- inputGATmessage(title = "Excluded areas", help = temp$help,
                                       helptitle = "inputGATmessage",
                                       helppage = "inputGATmessage", step = 6,
                                       msg = temp$msg, buttonopt = "Repeat")
        if (temp$cancel == "cancel") {
          exclist$var1 <- "repeat"
        } else if (temp$cancel == "back") {
          exclist$var1 <- "back"
          step <- step - 1
        }
      }

      if (!exclist$var1 %in% c("back", "cancel") & !temp$error) {
        if (temp$flagconfirm) {
          step <- 11
        } else {
          step <- step + 1
        }
      }
    } # end request exclusions (exclist)
    while (step == 6) { # radiobutton dialog to get merge type
      pb <- list(title = "NYSDOH GAT: identify merge type",
                 label = "Selecting the merge type.")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                              label = pb$label)

      # create a non-flagged subset of the main dataset to determine denominators
      temp$mapflag <- temp$mapdata[temp$mapdata$GATflag == 0, ]

      if (!exists("mergevars")) mergevars <- NULL
      if (is.null(mergevars$centroid)) mergevars <- NULL
      if (!exists("limitdenom")) limitdenom <- FALSE
      error <- TRUE

      while (error) {
        mergevars <- inputGATmerge(mapdata = temp$mapflag,
                                   aggvar = gatvars$aggregator1,
                                   aggvar2 = gatvars$aggregator2,
                                   step = step, limitdenom = limitdenom,
                                   mergevars = mergevars)
        error <- FALSE
        if (is.null(mergevars)) {
          x <- confirmGATquit()
          if (x == "quit") {
            mergevars <- list(similar1 = "cancel")
          } else {
            mergevars <- list(similar1 = "repeat", mergeopt1 = 0)
            error <- TRUE
          }
        }

        if (mergevars$similar1 == "back") {
          step <- step - 1 # back to exclusions
        } else if (mergevars$similar1 == "cancel") {
          mysettings$quit <- TRUE
          step <- 20
        } else if (mergevars$similar1 != "repeat" & mergevars$mergeopt1 != "0") {
          # quality control for similarity: force selection of different numerator
          # and denominator variables and check for bad data
          if (mergevars$mergeopt1 != "similar") {
            step = step + 1
          } else {
            if (mergevars$similar1 == mergevars$similar2) {
              temp$msg <- "Please select different variables for numerator and denominator."
              tcltk::tkmessageBox(title = "Please re-check variables", message = temp$msg,
                                  type = "ok", icon = "warning")
              error <- TRUE # re-run step
            } else {
              temp$go <- TRUE # passed first check
              # if similarvar2 has zero in denominator, require user to pick again
              # this should no longer be an issue
              temp$check <- is.finite(temp$mapflag[ , mergevars$similar1] /
                                      temp$mapflag[ , mergevars$similar2])
              if (FALSE %in% temp$check){
                temp$msg <- paste("The variable selected for the denominator cannot have",
                                  "values of zero. Please try again.")
                tcltk::tkmessageBox(title = "Please re-check variables", message = temp$msg,
                                    type = "ok", icon = "warning")
                error <- TRUE # re-run step
              } else {
                step <- step + 1
              }
            }
          }
        }
      }
    } # end request merge type (mergevars)
    while (step == 7) {
      pb <- list(title = "NYSDOH GAT: identify base population",
                 label = "Selecting the population file.")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                              label = pb$label)

      if (is.null(gatvars$popvar)) gatvars$popvar <- "NONE"
      if (is.null(filevars$popin)) filevars$popin <- filevars$userin

      if (mergevars$centroid == "population-weighted") {
        error <- TRUE
        while (error) {
          tempfiles <- locateGATshapefile(type = "population",
                                          myfile = filevars$popin, step = step)
          error <- FALSE
          if (tempfiles$userin == "cancel") {
            gatvars$popvar <- "back"
          } else {
            filevars$popin <- tempfiles$userin
            filevars$popfile <- tempfiles$filein
            filevars$poppath <- tempfiles$pathin
            temp$popdata <- foreign::read.dbf(paste0(filevars$popin, ".dbf"),
                                              as.is = TRUE)
            temp$popshp <- rgdal::readOGR(dsn = filevars$poppath,
                                       layer = filevars$popfile)
            temp$polys <- class(temp$popshp)
            temp$popnumvars <- checkGATvariabletypes(temp$popdata, type = "number")

            # add dialog specifying each issue
            # add check for shapefile type (if class(shp) )
            if (temp$polys != "SpatialPolygonsDataFrame") {
              # message: wrong kind of shapefile; repeat dialog
              temp$msg <- "The shapefile must contain polygons to be used."
              tcltk::tkmessageBox(title = "Shapefile invalid", type = "ok",
                                  icon = "error", message = temp$msg)
              error <- TRUE
            } else if (!is.null(temp$popnumvars)) {
              gatvars$popvar <- identifyGATpopulation(varlist = temp$popnumvars,
                                                      step = step, var = gatvars$popvar)
            }
          }
          if (gatvars$popvar == "back") {
            step <- step - 1
          } else {
            if (temp$flagconfirm) {
              step <- 11
            } else {
              step <- step + 1
            }
          }
          rm(tempfiles)
        }
      } else {
        if (temp$flagconfirm) {
          step <- 11
        } else {
          step <- step + 1
        }
      }
    } # end request population weighting
    while (step == 8) { # step 9: get rate settings
      pb <- list(title = "NYSDOH GAT: identify rate",
                 label = "Selecting the rate details.")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                              label = pb$label)

      if (length(temp$numerics) > 1) {
        error <- TRUE

        if (!exists("ratevars")) ratevars <- list()

        while (error) {
          ratevars <- inputGATrate(mapdata = temp$mapflag,
                                   limitdenom = limitdenom,
                                   step = step, ratevars = ratevars)
          error <- FALSE

          # returns list(multiplier, ratename, numerator, denominator, colorscheme)
          if (!exists("ratename", ratevars)) {
            x <- confirmGATquit()
            if (x == "quit") {
              ratevars <- list(ratename = "cancel")
            } else {
              error <- TRUE
              ratevars <- list(ratename = "gat_rate")
            }
          }
          if (ratevars$ratename == "cancel") {
            mysettings$quit <- TRUE
            step <- 20
          } else if (ratevars$ratename == "back") {
            step <- step - 1
          } else if (!error) {
            if (ratevars$ratename == "no_rate") {
              #temp$msg <- "You have chosen not to calculate a rate."
            } else {
              # quality control - force numeric
              while (grepl("[^0-9.,-]", ratevars$multiplier)) {
                gats <- list(title = paste("Multiplier for", ratevars$ratename),
                             msg = paste0("Please enter a valid number for the ",
                                          "multiplier for ", ratevars$ratename, "."),
                             help = paste0("Enter a valid number. \n",
                                           "  \u2022  To continue,  click 'Next >'. \n",
                                           "  \u2022  To return to rate settings, click '< Back'.",
                                           "  \u2022  To quit GAT, click 'Cancel'."))
                ratevars$multiplier <- inputGATvalue(title = gats$title, help = gats$help,
                                                     message = gats$msg, defaulttext = "10,000",
                                                     helppage = "inputGATvalue", step = step)
                if (ratevars$multiplier == "back") {
                  ratevars$ratename <- "back"
                  ratevars$multiplier <- 1
                } else if (ratevars$multiplier == "cancel") {
                  ratevars$ratename <- "cancel"
                  ratevars$multiplier <- 1
                  mysettings$quit <- TRUE
                  step <- 20
                }
              }
              ratevars$multiplier <- as.numeric(gsub(",", "", ratevars$multiplier))

              # quality control - force selection of different numerator and denominator
              # create a dialog specifically for this?
              if (!exists("numerator", ratevars) | !exists("denominator", ratevars) |
                  length(ratevars$numerator) == 0 | length(ratevars$denominator) == 0) {
                temp$msg <- "Please select different variables for numerator and denominator"
                tcltk::tkmessageBox(title = "Please re-check variables", message = temp$msg,
                                    type = "ok", icon = "warning")
                ratevars$ratename <- "gat_rate"
                error <- TRUE
              } else if (ratevars$numerator == ratevars$denominator) {
                temp$msg <- "Please reselect the numerator and denominator"
                tcltk::tkmessageBox(title = "Please re-check variables", message = temp$msg,
                                    type = "ok", icon = "warning")
                ratevars$ratename <- "gat_rate"
                error <- TRUE
              }
              #temp$msg <- paste0("You have chosen to calculate the rate ",
              #                   ratevars$ratename, " from ", ratevars$numerator,
              #                   " and ", ratevars$denominator, " with multiplier ",
              #                   format(as.numeric(ratevars$multiplier), big.mark=",",
              #                          scientific=FALSE),
              #                   " using the color scheme ", ratevars$colorname,
              #                   ". Is this correct?")

            }
          }
        }
        #if (!ratevars$ratename %in% c("back", "cancel")) {
        #  temp$title <- "Confirm your rate calculation settings"
        #  temp$x <- tcltk::tkmessageBox(title = temp$title, message = temp$msg,
        #                                type = "okcancel", icon = "question")
        #  temp$confirm <- tcltk::tclvalue(temp$x)

        #  if (temp$confirm == "ok") {
        #    if (temp$flagconfirm) {
        #      step <- 11
        #    } else {
        #      step <- step + 1
        #    }
        #  }
        #}
      } else {
        # message: wrong kind of shapefile; repeat dialog
        temp$msg <- "There are not enough numeric variables to calculate a rate."
        tcltk::tkmessageBox(title = "Rate calculation not possible", type = "ok",
                            icon = "warning", message = temp$msg)
        if (temp$flagconfirm) {
          step <- 11
        } else {
          step <- step + 1
        }
        ratevars <- list(ratename = "no_rate")
      }
    } # end request rate (ratevars)
    while (step == 9) {
      # add an option to save the KML file
      pb <- list(title = "NYSDOH GAT: save KML?",
                 label = "Identifying whether to save a KML file.")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title, label = pb$label)

      temp$kml <- saveGATkml(step = step)

      if (temp$kml == "Yes") {
        gatvars$savekml <- TRUE # save the kml
        if (temp$flagconfirm) {
          step <- 11
        } else {
          step <- step + 1
        }
      } else if (temp$kml == "No") {
        gatvars$savekml <- FALSE # do not save the kml
        if (temp$flagconfirm) {
          step <- 11
        } else {
          step <- step + 1
        }
      } else if (temp$kml == "quit") {
        step <- 20
      }else {
        step <- step - 1
      }
    } # end request save KML (savekml)
    while (step == 10) {
      # identify the save files' name and location
      pb <- list(title = "NYSDOH GAT: identify save file",
                 label = "Identifying the name and location of your save file.")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title, label = pb$label)

      saves <- saveGATfiles()
      filevars$userout <- saves$userout
      filevars$fileout <- saves$fileout
      filevars$pathout <- saves$pathout

      if (filevars$fileout == "cancel") {
        mysettings$quit <- TRUE
        step <- 20
      } else {
        step <- step + 1
      }
    } # end request save file (fileout)
    while (step == 11) { # add dialog to confirm merge settings
      pb <- list(title = "NYSDOH GAT: confirm settings",
                 label = "Confirming your GAT settings.")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                              label = pb$label)
      temp$flagconfirm <- TRUE
      error <- TRUE
      gatvars$numrow <- nrow(temp$mapdata)
      while (error) {
        temp$cancel <- confirmGATbystep(gatvars = gatvars, ratevars = ratevars,
                                        exclist = exclist, mergevars = mergevars,
                                        filevars = filevars, step = step)
        error <- FALSE
        if (is.null(temp$cancel)) {
          x <- confirmGATquit()
          if (x == "quit") {
            temp$cancel <- "Yes"
            mysettings$quit <- TRUE
          } else {
            error <- TRUE
          }
        }
      }
      if (temp$cancel %in% c("Yes", "None")) {
        step <- 20 # done with user input
        myshps$original <- temp$shp
        temp$shp@data <- temp$mapdata
      } else if (temp$cancel == "back") {
        step <- step - 1 # go back one
      } else if (temp$cancel == "cancel") {
        step <- 1 # if no, start again
      } else if (grepl("[0-9]", temp$cancel)) {
        step <- as.numeric(gsub("[^0-9]", "", temp$cancel))
      }
    } # end request settings confirmation (filein)
  } # end while (step < 20)
  rm(temp)
  #     end user input ####

  if (!mysettings$quit) { # everything from here onward is automated
    #  3. read in shapefile ####
    step <- 14 # reset after the while loop
    pb <- list(title = "NYSDOH GAT: processing the shapefile",
               label = paste0("Reading in map from ", filevars$filein, "."))
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title, label = pb$label)

    # reads map in as spatialpolygonsdataframe with projection information
    # myshps$original <- rgdal::readOGR(dsn = filevars$pathin,
    #                                   layer = filevars$filein,
    #                                   stringsAsFactors = FALSE)
    # myshps$original@data <- foreign::read.dbf(paste0(filevars$userin, ".dbf"),
    #                                           as.is = TRUE) # maintains numerics
    if ("GATy" %in% names(myshps$original@data)) {
      myshps$original@data <- myshps$original@data[,
        names(myshps$original@data)[!names(myshps$original@data) %in%
          c("old_GATx", "old_GATy", "old_GATnumIDs", "old_GATcratio",
            "old_GATflag")]]
      for (i in 1: ncol(myshps$original@data)) {
        # possibly these can be removed entirely, but need to verify
        # if removing, inform user with warning dialog
        # not sure how to handle flag variable - rename it?

        if (names(myshps$original@data)[i] %in%
            c("GATx", "GATy", "GATnumIDs", "GATcratio", "GATflag")) {
          names(myshps$original@data)[i] <-
            paste0("old_", names(myshps$original@data)[i])
        }
      }
    }
    # housekeeping
    if (gatvars$aggregator2 == "NONE") {
      gatvars$aggregator2 <- gatvars$aggregator1
    }
    rm(error)
    gatvars$maxvalue1 <- as.numeric(gsub(",", "", gatvars$maxvalue1))
    gatvars$maxvalue2 <- as.numeric(gsub(",", "", gatvars$maxvalue2))
    gatvars$minvalue1 <- as.numeric(gsub(",", "", gatvars$minvalue1))
    gatvars$minvalue2 <- as.numeric(gsub(",", "", gatvars$minvalue2))

    if (gatvars$myidvar == "missing") {
      myshps$original@data$temp_id <- paste0("ID_", 1:nrow(myshps$original@data))
      gatvars$myidvar <- "temp_id"
    }
    myshps$original <- sp::spChFIDs(myshps$original,
                                    myshps$original@data[, gatvars$myidvar])

    # proj4string(myshp) # find if this map is lat/long or not (input projection)
    mapvars <- list(projection = grepl("longlat",
                                       sp::proj4string(myshps$original),
                                       fixed = TRUE),  # returns logical vector
                    # suggested to avoid get.Pcent.
                    centroids = sp::coordinates(myshps$original))
    colnames(mapvars$centroids) <- c("GATx", "GATy")

    # if projection is lat/lon, projection = TRUE, otherwise FALSE
    # default to not lat/long if something goes wrong
    if (is.na(mapvars$projection)) mapvars$projection <- FALSE

    # create a flag variable
    myshps$original@data$GATflag <- 0 # all areas are included in the merge

    # exclusions: for merge minimum violated, flag = 10
    myshps$original@data$GATflag <- calculateGATflag(exclist, myshps$original@data)
    myshps$original@data$GATflag <-
      ifelse(myshps$original@data[, gatvars$aggregator1] > gatvars$maxvalue1, 5,
             myshps$original@data$GATflag)
    if (!gatvars$aggregator2 == gatvars$aggregator1) {
      myshps$original@data$GATflag <-
        ifelse(myshps$original@data[, gatvars$aggregator2] > gatvars$maxvalue2, 5,
               myshps$original@data$GATflag)
    }

    #  4. run aggregation loop ####
    step <- step + 1
    pb$label = paste0("Aggregating ", filevars$filein, ".")
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title, label = pb$label)

    gatvars$popwt <- mergevars$centroid == "population-weighted"
    aggvars <- defineGATmerge(area = myshps$original, gatvars = gatvars,
                              mergevars = mergevars, filevars = filevars,
                              pwrepeat = pwrepeat, adjacent = adjacent)

    #  5. aggregate areas ####
    step <- step + 1
    pb$label = paste("Completed", aggvars$newregno, "mergings.")
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title, label = pb$label)

    myshps$aggregated <- mergeGATareas(ratevars = ratevars, aggvars = aggvars,
                                       idvar = gatvars$myidvar,
                                       myshp = myshps$original)


    #  6. calculate compactness ratio ####
    step <- step + 1
    pb$label = "Checking compactness ratio."
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title, label = pb$label)

    # to get maximum distance (diameter of circle): max(dist(test1))
    temp <- list(cratio = calculateGATcompactness(myshps$aggregated),
                 ncol = ncol(myshps$aggregated))

    myshps$compact <- maptools::spCbind(myshps$aggregated, temp$cratio)
    names(myshps$compact)[temp$ncol+1] <- "GATcratio"

    #  7. map first variable: before and after ####
    step <- step + 1
    pb <- list(title = "NYSDOH GAT: mapping variables",
               label = paste0("Mapping ", gatvars$aggregator1, ". Please wait."))
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title, label = pb$label)

    # create a list to record plots to be saved to pdf at the end.
    myplots <- list()

    # plot first aggregation variable
    temp <- defineGATmapclasses(myshps$original, myshps$aggregated,
                                gatvars$aggregator1)

    mapvars$title <- paste(gatvars$aggregator1, "Before Merging")
    mapvars$colcode1before <- temp$colcode1before
    mapvars$colcode1after <- temp$colcode1after

    myplots$aggregator1before <- plotGATmaps(area = myshps$original,
                                             var = gatvars$aggregator1,
                                             title.main = mapvars$title,
                                             colcode = mapvars$colcode1before,
                                             mapstats = TRUE)

    # find the new maximums after aggregation
    mapvars$titlemain = paste(gatvars$aggregator1, "After Merging")
    mapvars$titlesub = paste("Aggregation values:",
                             numformat(gatvars$minvalue1), "to",
                             numformat(as.numeric(gsub(",", "",
                                                       gatvars$maxvalue1))),
                             gatvars$aggregator1)
    if (exclist$var1 != "NONE") {
      mapvars$titlesub <- paste(mapvars$titlesub, "\nExclusion criteria: ",
                        exclist$var1, exclist$math1, numformat(exclist$val1))
    }
    if (exclist$var2 != "NONE") {
      mapvars$titlesub <- paste(mapvars$titlesub, ";",
                                exclist$var2, exclist$math2,
                        numformat(exclist$val2))
    }
    if (exclist$var3 != "NONE") {
      mapvars$titlesub <- paste(mapvars$titlesub, ";",
                                exclist$var3, exclist$math3,
                        numformat(exclist$val3))
    }



    myplots$aggregator1after <- plotGATmaps(area = myshps$aggregated,
                                            var = gatvars$aggregator1,
                                            title.main = mapvars$titlemain,
                                            title.sub = mapvars$titlesub,
                                            colcode = mapvars$colcode1after,
                                            after = TRUE, mapstats = TRUE)

    #  8. map second variable: before and after ####
    step <- step + 1
    if (!gatvars$aggregator2 %in% c("NONE", gatvars$aggregator1)) {
      pb$label = paste("Mapping ", gatvars$aggregator2, ". Please wait.")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title, label = pb$label)

      # plot second aggregation variable, if relevant
      temp <- defineGATmapclasses(myshps$original, myshps$aggregated,
                                  gatvars$aggregator2)

      mapvars$title <- paste(gatvars$aggregator2, "Before Merging")
      mapvars$colcode2before <- temp$colcode2before
      mapvars$colcode2after <- temp$colcode2after

      myplots$aggregator2before <- plotGATmaps(area = myshps$original,
                                               var = gatvars$aggregator2,
                                               title.main = mapvars$title,
                                               colcode = mapvars$colcode2before,
                                               mapstats = TRUE)

      mapvars$titlemain = paste(gatvars$aggregator2, "After Merging")
      mapvars$titlesub = paste("Aggregation values:",
                               numformat(gatvars$minvalue2), "to",
                               numformat(as.numeric(gsub(",", "",
                                                         gatvars$maxvalue2))),
                               gatvars$aggregator2)
      if (exclist$var1 != "NONE") {
        mapvars$titlesub <- paste(mapvars$titlesub, "\nExclusion criteria: ",
                                  exclist$var1, exclist$math1,
                                  numformat(exclist$val1))
      }
      if (exclist$var2 != "NONE") {
        mapvars$titlesub <- paste(mapvars$titlesub, ";", exclist$var2, exclist$math2,
                                  numformat(exclist$val2))
      }
      if (exclist$var3 != "NONE") {
        mapvars$titlesub <- paste(mapvars$titlesub, ";", exclist$var3, exclist$math3,
                                  numformat(exclist$val3))
      }

      myplots$aggregator2after <- plotGATmaps(area = myshps$aggregated,
                                              var = gatvars$aggregator2,
                                              title.main = mapvars$titlemain,
                                              title.sub = mapvars$titlesub,
                                              colcode = mapvars$colcode2after,
                                              after = TRUE, mapstats = TRUE)
    }

    #  9. map differences between old and new areas ####
    step <- step + 1
    pb$label = "Mapping differences before and after merging."
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title, label = pb$label)

    # plot new and old on same map
    myplots$compare <-  plotGATcompare(areaold = myshps$original,
                                       areanew = myshps$aggregated,
                                       mergevars = mergevars,
                                       gatvars = gatvars)

    # 10. map compactness ratio ####
    step <- step + 1
    pb$label = "Mapping compactness ratio after merging."
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title, label = pb$label)

    # create thematic map of compactness ratios
    gats <- list(title.main = "Compactness Ratio After Merging",
                 title.sub = "1=most compact, 0=least compact")
    myplots$compactness <- plotGATmaps(area = myshps$compact,
                                       var = "GATcratio", clr = "YlOrBr",
                                       title.main = gats$title.main,
                                       title.sub = gats$title.sub,
                                       ratemap = TRUE)

    # 11. map rates if needed ####
    step <- step + 1
    if (ratevars$ratename != "no_rate") { # map the rate, choropleth map
      pb$label = paste0("Mapping rate variable ", ratevars$ratename, ".")
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                              label = pb$label)

      gats <- list(title = paste(ratevars$ratename, "per",
                                 format(as.numeric(ratevars$multiplier),
                                        big.mark = ",", scientific=FALSE),
                                 "after merging"),
                   sub = paste("Rate calculation:", ratevars$ratename, "=",
                               format(as.numeric(ratevars$multiplier),
                                      big.mark = ",", scientific=FALSE), "*",
                               ratevars$numerator, "/", ratevars$denominator))
      if (exclist$var1 != "NONE") {
        gats$sub <- paste(gats$sub, "\nExclusion criteria: ",
                          exclist$var1, exclist$math1,
                          format(exclist$val1, big.mark=",", scientific=FALSE))
      }
      if (exclist$var2 != "NONE") {
        gats$sub <- paste(gats$sub, ";", exclist$var2, exclist$math2,
                          format(exclist$val2, big.mark=",", scientific=FALSE))
      }
      if (exclist$var3 != "NONE") {
        gats$sub <- paste(gats$sub, ";", exclist$var3, exclist$math3,
                          format(exclist$val3, big.mark=",", scientific=FALSE))
      }

      myplots$rate <- plotGATmaps(area = myshps$aggregated, var = ratevars$ratename,
                                  clr = ratevars$colorscheme,
                                  title.sub = gats$sub,
                                  title.main = gats$title, ratemap = TRUE,
                                  mapstats = TRUE)
    } # end mapping new rate

    # 12. save old shapefile ####
    step <- step + 1
    pb <- list(title = "NYSDOH GAT: saving files",
               label = paste("Writing the original shapfile to",
                             paste0(filevars$fileout, "in")))
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title, label = pb$label)

    # create crosswalk of old and new ids
    myshps$crosswalk <- myshps$original
    myshps$crosswalk@data <- cbind(myshps$original@data,
                                   data.frame(GATid = aggvars$IDlist))

    # warnings don't make sense; they say data not written successfully,
    # but shapefile is fine and data match original file
    rgdal::writeOGR(myshps$crosswalk, filevars$pathout,
                    paste0(filevars$fileout, "in"),
                    driver = "ESRI Shapefile", verbose = TRUE,
                    overwrite_layer = TRUE)

    # 13. save new shapefile ####
    step <- step + 1
    pb$label = paste("Writing the merged shapfile to", filevars$fileout)
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title, label = pb$label)

    # export the map as a shapefile
    rgdal::writeOGR(myshps$compact, filevars$pathout,
                      filevars$fileout, driver = "ESRI Shapefile",
                      verbose = TRUE, overwrite_layer = TRUE)
    # large areas throw warnings that appear unfounded

    # 14. save maps to pdf ####
    step <- step + 1
    pb$label = paste0("Writing the plots to ", filevars$fileout, "plots.pdf.")
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                            label = pb$label)

    # save the plots to a pdf file
    pdf(paste0(filevars$userout, "plots.pdf"), onefile=TRUE, width = 10,
        height = 7)
    for (myplot in myplots) {
      if (class(myplot) == "recordedplot") replayPlot(myplot)
    } # only saves plots that exist
    dev.off() # need to close pdf file

    rm(myplots)

    # 15. save kml file ####
    if (gatvars$savekml==TRUE) { # now includes descriptions
      step <- step + 1
      pb$label = "Writing the KML file."
      tcltk::setTkProgressBar(tpb, value = step, title = pb$title,
                              label = pb$label)

      writeGATkml(myshp = myshps$compact, filename = filevars$fileout,
                  filepath = filevars$pathout, myidvar = gatvars$myidvar)
    }
    # 16. save log file ####
    step <- step + 1
    pb$label = paste0("Writing the log to ",
                      paste0(filevars$fileout, ".log"), ".")
    tcltk::setTkProgressBar(tpb, value = step, title = pb$title, label = pb$label)

    mysettings$exists = file.exists(paste0(filevars$userout, ".shp"))

    # include projection? number of aggregations? rate calculations?
    # recode to read in lists (or pre-create chunks) instead of individual values?
    writeGATlog(gatvars = gatvars, aggvars = aggvars, filevars = filevars,
                mysettings = mysettings, area = myshps$original,
                mergevars = mergevars, ratevars = ratevars, exclist = exclist)

    step <- step + 1
    pb$label = "GAT is finished."
    tcltk::setTkProgressBar(tpb, value = 27, title = pb$title, label = pb$label)

    # save relevant objects
    save(file = paste0(filevars$userout, "settings.Rdata"),
         list = c("gatvars", "aggvars", "filevars", "mergevars", "ratevars",
                  "exclist"))

    if (mysettings$exists) {
      msg <- paste0("NYS GAT is finished. Your files were saved to ",
                    filevars$pathout,
                    ". \nPlease see the log file for more details.")
      tcltk::tkmessageBox(title = "GAT finished", type = "ok",
                          icon = "info", message = msg)

      msg <- paste0("\n\nThe following files have been written to the folder \n",
                    filevars$pathout, ": \n  ",
                    filevars$fileout, ".dbf \n  ",
                    filevars$fileout, ".prj \n  ",
                    filevars$fileout, ".shp \n  ",
                    filevars$fileout, ".shx \n  ",
                    filevars$fileout, "in.dbf \n  ",
                    filevars$fileout, "in.prj \n  ",
                    filevars$fileout, "in.shp \n  ",
                    filevars$fileout, "in.shx \n  ",
                    filevars$fileout, "plots.pdf \n  ",
                    filevars$fileout, ".log \n  ",
                    filevars$fileout, "settings.Rdata \n  ")
      if (gatvars$savekml==TRUE) msg <- paste0(msg, filevars$fileout, ".kml \n")
      msg <- paste0(msg, "\nSee the log file for more details.")

      message(msg)
    } else {
      # the shapefile failed to write
      msg <- "Something went wrong. Your shapefiles were not saved."
      tcltk::tkmessageBox(title = "Shapefile save failed", type = "ok",
                          icon = "error", message = msg)
    }

  } else {
    # "else" occurs only if GAT is cancelled in the input phase
    # there are now redundant warnings; function-level are commented out
    msg <- "You have chosen to cancel GAT."
    tcltk::tkmessageBox(title = "GAT cancelled",
                        message = msg,
                        type = "ok", icon = "warning")
  }
  # end GAT ####
  close(tpb)
}

