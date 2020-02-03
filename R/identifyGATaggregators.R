#' Identify and Check GAT Aggregation Variables
#'
#' @description
#' This function opens a dialog window for the user to select which variables
#' in the shapefile should be aggregated and what their minimum and maximum
#' values should be using the function \code{\link{inputGATaggregators}}. It
#' checks the resulting numeric values to ensure they are numbers and if
#' "NONE" is selected for the second aggregation variable, it assigns the
#' values for the first aggregation variable to the values for the second
#' aggregation variable.
#'
#'
#' @param mapdata A data frame.
#' @param step    An integer step in the GAT program, for help reference.
#' @param agglist The list of aggregation variables, with minimum and maximum
#'                values, if pre-selected.
#'
#' @examples
#'
#' \donttest{
#' # identify variable to aggregate
#' identifyGATaggregators(
#'   mapdata = hftown@data
#' )
#' }
#'
#' @export

# I want to shorten this code, since I write the same thing three times ...

identifyGATaggregators <- function(mapdata, step = 4, agglist = NULL) {
  ## enter settings ####
  error <- TRUE

  while(error) {
    if (is.null(agglist)) {
      agglist <- inputGATaggregators(mapdata = mapdata, step = step)
    } else {
      agglist <- inputGATaggregators(mapdata = mapdata, step = step,
                                     var1 = agglist$var1, var2 = agglist$var2,
                                     min1 = agglist$minval1, min2 = agglist$minval2,
                                     max1 = agglist$maxval1, max2 = agglist$maxval2)
    }

    ## if settings fail ####
    if (is.null(agglist)) {
      x <- confirmGATquit()
      if (x == "quit") {
        agglist <- list(var1 = "cancel", minval1 = "5,000", maxval1 = "none",
                        var2 = "NONE", minval2 = "none", maxval2 = "none")
        error <- FALSE
      } else {
        agglist <- list(var1 = "repeat", minval1 = "5,000", maxval1 = "none",
                        var2 = "NONE", minval2 = "none", maxval2 = "none")
      }
    } else if (length(agglist) == 0) {
      agglist <- list(var1 = "repeat", minval1 = "5,000", maxval1 = "none",
                      var2 = "NONE", minval2 = "none", maxval2 = "none")
    } else {
      error <- FALSE
    }
  }

  if (agglist$var1 == agglist$var2) {
    msg <- paste("Please select different variables for the first and",
                 "second aggregation variables. If you do not want a",
                 "second aggregation variable, please select 'NONE'.")
    tcltk::tkmessageBox(title = "Please re-check variables", message = msg,
                        type = "ok", icon = "warning")
    agglist <- inputGATaggregators(mapdata = mapdata, step = step,
                        var1 = agglist$var1, var2 = agglist$var2,
                        min1 = agglist$minval1, min2 = agglist$minval2,
                        max1 = agglist$maxval1, max2 = agglist$maxval2)
  }

  if (!agglist$var1 %in% c("cancel", "repeat") & !error) {
    # check that all numbers are actual numbers
    # vector should include only relevant var and not ignored
    # if ignored, assign min or total of var
    if (agglist$maxval1 == "none") {
      agglist$maxval1 = sum(mapdata[, agglist$var1], na.rm = TRUE)
    } else {
      while (grepl("[^0-9.,-]", agglist$maxval1) | is.na(agglist$maxval1)) {
        gats <- list(title = paste("Maximum value for", agglist$var1),
                     msg = paste0("Please enter a valid maximum value for ",
                                  agglist$var1, "."),
                     help = paste0("Enter a valid number that is greater than ",
                                   agglist$minval1, ". \n",
                                   "  \u2022  To continue,  click 'Next >'. \n",
                                   "  \u2022  To return to boundary selection, click '< Back'.",
                                   "  \u2022  To quit GAT, click 'Cancel'."),
                     defaultvalue = sum(mapdata[, agglist$var1], na.rm = TRUE))
        agglist$maxval1 <- inputGATvalue(title = gats$title, help = gats$help,
                                         message = gats$msg,
                                         defaulttext = gats$defaultvalue,
                                         helppage = "inputGATvalue", step = step)
        if (is.null(agglist$maxval1)) {
          x <- confirmGATquit()
          if (x == "quit") {
            agglist$maxval1 <- "cancel"
          } else {
            agglist$maxval1 <- "repeat"
          }
        }
        if (agglist$maxval1 == "back") {
          agglist$var1 <- "repeat"
          error <- TRUE
          agglist$maxval1 <- 0
        } else if (agglist$maxval1 == "cancel") {
          agglist$var1 <- "cancel"
          agglist$maxval1 <- 0
        } else if (agglist$maxval1 <= agglist$minval1) {
          error <- TRUE
        }
      }
    }

    if (agglist$var2 == "NONE") {
      agglist$maxval2 <- agglist$maxval1
      agglist$minval2 <- agglist$minval1
    } else {
      if (agglist$minval2 == "none") {
        agglist$minval2 = min(mapdata[, agglist$var2], na.rm = TRUE)
      } else {
        while (grepl("[^0-9.,-]", agglist$minval2) | is.na(agglist$minval2)) {
          gats <- list(title = paste("Minimum value for", agglist$var2),
                       msg = paste0("Please enter a valid minimum value for ",
                                    agglist$var2, "."),
                       help = paste0("Enter a valid number. \n",
                                     "  \u2022  To continue,  click 'Next >'. \n",
                                     "  \u2022  To return to boundary selection, click '< Back'.",
                                     "  \u2022  To quit GAT, click 'Cancel'."),
                       defaultvalue = min(mapdata[, agglist$var2], na.rm = TRUE))
          agglist$minval2 <- inputGATvalue(title = gats$title, help = gats$help,
                                           message = gats$msg,
                                           defaulttext = gats$defaultvalue,
                                           helppage = "inputGATvalue", step = step)
          if (is.null(agglist$minval2)) {
            x <- confirmGATquit()
            if (x == "quit") {
              agglist$minval2 <- "cancel"
            } else {
              agglist$minval2 <- "repeat"
            }
          }
          if (agglist$minval2 == "back") {
            agglist$var1 <- "repeat"
            error <- TRUE
            agglist$minval2 <- 0
          } else if (agglist$minval2 == "cancel") {
            agglist$var1 <- "cancel"
            agglist$minval2 <- 0
          }
        }
      }
      if (agglist$maxval2 == "none") {
        agglist$maxval2 = sum(mapdata[, agglist$var2], na.rm = TRUE)
      } else {
        while (grepl("[^0-9.,-]", agglist$maxval2) | is.na(agglist$maxval2)) {
          gats <- list(title = paste("Maximum value for", agglist$var2),
                       msg = paste0("Please enter a valid maximum value for ",
                                    agglist$var2, "."),
                       help = paste0("Enter a valid number that is greater than ",
                                     agglist$minval2, ". \n",
                                     "  \u2022  To continue,  click 'Next >'. \n",
                                     "  \u2022  To return to boundary selection, click '< Back'.",
                                     "  \u2022  To quit GAT, click 'Cancel'."),
                       defaultvalue = sum(mapdata[, agglist$var2], na.rm = TRUE))
          agglist$maxval2 <- inputGATvalue(title = gats$title, help = gats$help,
                                           message = gats$msg,
                                           defaulttext = gats$defaultvalue,
                                           helppage = "inputGATvalue", step = step)
          if (is.null(agglist$maxval2)) {
            x <- confirmGATquit()
            if (x == "quit") {
              agglist$maxval2 <- "cancel"
            } else {
              agglist$maxval2 <- "repeat"
            }
          }
          if (agglist$maxval2 == "back") {
            agglist$var1 <- "repeat"
            error <- TRUE
            agglist$maxval2 <- 0
          } else if (agglist$maxval2 == "cancel") {
            agglist$var1 <- "cancel"
            agglist$maxval2 <- 0
          } else if (agglist$maxval2 <= agglist$minval2) {
            error <- TRUE
          }
        }
      }
    }
  }
  return(agglist)
}
