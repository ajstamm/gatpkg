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
#' @param shp        Spatial layer.
#' @param step       Integer step in GAT, for help reference.
#' @param agglist    List of aggregation variables, with minimum and maximum values,
#'                   if pre-selected.
#' @param backopt    Boolean denoting whether to include the back button.
#' @param quitopt    Text string for the cancel button.
#' @param bgcol      Text string containing UI background color.
#' @param buttoncol  Text string containing UI button color.
#'
#' @examples
#'
#' if (interactive()) {
#' identifyGATaggregators(shp = hftown)
#' }
#'
#' @export

# I want to shorten this code, since I write the same thing three times ...

identifyGATaggregators <- function(shp, step = 4, agglist = NULL,
                                   backopt = TRUE,
                                   bgcol = "lightskyblue3", quitopt = "Quit",
                                   buttoncol = "cornflowerblue") {
  ## enter settings ####
  error <- TRUE

  while(error) {
    if (is.null(agglist)) {
      agglist <- inputGATaggregators(shp = shp, step = step, backopt = backopt)
    } else if (agglist$var1 %in% c("back", "cancel", "repeat")) {
      agglist <- inputGATaggregators(shp = shp, step = step, backopt = backopt)
    } else {
      if (agglist$var2 %in% c(agglist$var1, "back", "cancel", "repeat")) {
        agglist$var2 <- "NONE"
      }
      if (agglist$var2 == "NONE") {
        agglist$minval2 <- "none"
        agglist$maxval2 <- "none"
      } else {
        if (agglist$minval2 == min(data.frame(shp)[, agglist$var2])) {
          agglist$minval2 <- "none"
        }
        if (agglist$maxval2 == sum(data.frame(shp)[, agglist$var2])) {
          agglist$maxval2 <- "none"
        }
      }
      if (agglist$maxval1 == sum(data.frame(shp)[, agglist$var1])) {
        agglist$maxval1 <- "none"
      }

      agglist <- inputGATaggregators(shp = shp, step = step, backopt = backopt,
                                     var1 = agglist$var1, var2 = agglist$var2,
                                     min1 = agglist$minval1, min2 = agglist$minval2,
                                     max1 = agglist$maxval1, max2 = agglist$maxval2,
                                     bgcol = bgcol, quitopt = quitopt,
                                     buttoncol = buttoncol)
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
    } else if (agglist$var1 %in% c("back", "cancel")) {
      error <- FALSE
    } else {
      error <- FALSE
      # issues with variable 1 ####
      varerror <- FALSE
      if (agglist$var1 == "repeat") {
        error <- TRUE
      } else if (agglist$var1 == agglist$var2) {
        msg <- paste("Please select different variables for the first and",
                     "second aggregation variables. If you do not want a",
                     "second aggregation variable, please select 'NONE'.")
        tcltk::tkmessageBox(title = "Please re-check variables", message = msg,
                            type = "ok", icon = "warning")
        error <- TRUE
        varerror <- TRUE
      }
      if (!varerror) {
        # issues with minimum value 1 ####
        while (is.na(suppressWarnings(as.numeric(gsub(",", "", agglist$minval1))))) {
          gats <- list(title = paste("Minimum value for", agglist$var1),
                       help = paste0("Enter a valid number. \n",
                                     "  \u2022  To continue,  click 'Next >'. \n",
                                     "  \u2022  To return to boundary selection, click '< Back'.",
                                     "  \u2022  To quit GAT, click '", quitopt, "'."),
                       minvalue = min(data.frame(shp)[, agglist$var1], na.rm = TRUE))
          gats$msg = paste0("Please enter a valid minimum value for ",
                            agglist$var1, "\n", "that is at least ", gats$minvalue, ".")
          agglist$minval1 <- inputGATvalue(title = gats$title, help = gats$help,
                                           message = gats$msg,
                                           defaulttext = gats$minvalue,
                                           helppage = "inputGATvalue",
                                           step = step, backopt = backopt,
                                           bgcol = bgcol, quitopt = quitopt,
                                           buttoncol = buttoncol)
          if (is.null(agglist$minval1)) {
            x <- confirmGATquit()
            if (x == "quit") {
              agglist$var1 <- "cancel"
              agglist$minval1 <- 0
              error <- FALSE
            } else {
              agglist$minval1 <- "repeat"
            }
          }
          if (agglist$minval1 == "back") {
            agglist$var1 <- "repeat"
            agglist$minval1 <- 0
            error <- TRUE
          } else if (agglist$minval1 == "cancel") {
            agglist$var1 <- "cancel"
            agglist$minval1 <- 0
            error <- FALSE
          }
        }
        if (as.numeric(gsub(",", "", agglist$minval1)) <
            min(data.frame(shp)[, agglist$var1], na.rm = TRUE)) {
          msg <- paste("Your selected minimum value of", agglist$minval1,
                       "is lower than the minimum value of", agglist$var1, "(",
                       min(data.frame(shp)[, agglist$var1], na.rm = TRUE), "),",
                       "so no areas would be merged. Please change either your",
                       "aggregation variable or your desired minimum value.")
          tcltk::tkmessageBox(title = "Please change selections", message = msg,
                              type = "ok", icon = "info")
          error <- TRUE
        }


        # issues with maximum value 1 ####
        if (agglist$maxval1 != "none") {
          while (is.na(suppressWarnings(as.numeric(gsub(",", "", agglist$maxval1))))) {
            gats <- list(title = paste("Maximum value for", agglist$var1),
                         msg = paste0("Please enter a valid maximum value for ",
                                      agglist$var1, "\n", "that is greater than ",
                                      agglist$minval1, "."),
                         help = paste0("Enter a valid number that is greater than ",
                                       agglist$minval1, ". \n",
                                       "  \u2022  To continue,  click 'Next >'. \n",
                                       "  \u2022  To return to boundary selection, click '< Back'.",
                                       "  \u2022  To quit GAT, click '", quitopt, "'."),
                         maxvalue = sum(data.frame(shp)[, agglist$var1], na.rm = TRUE))
            agglist$maxval1 <- inputGATvalue(title = gats$title, help = gats$help,
                                             message = gats$msg,
                                             defaulttext = gats$maxvalue,
                                             helppage = "inputGATvalue",
                                             step = step, backopt = backopt,
                                             bgcol = bgcol, quitopt = quitopt,
                                             buttoncol = buttoncol)
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
            }
          }
          if (suppressWarnings(as.numeric(gsub(",", "", agglist$maxval1))) <=
              suppressWarnings(as.numeric(gsub(",", "", agglist$minval1)))) {
            msg <- paste("Please select a maximum value that is larger than the",
                         "minimum value for your first aggregation variable.",
                         "If you do not want to define a maximum value, please",
                         "type 'none'.")
            tcltk::tkmessageBox(title = "Please re-check minimum and maximum values",
                                message = msg, type = "ok", icon = "warning")
            error <- TRUE
          }
        }
        # issues with minimum value 2 ####
        if (agglist$minval2 != "none") {
          while (is.na(suppressWarnings(as.numeric(gsub(",", "", agglist$minval2))))) {
            gats <- list(title = paste("Minimum value for", agglist$var2),
                         help = paste0("Enter a valid number. \n",
                                       "  \u2022  To continue,  click 'Next >'. \n",
                                       "  \u2022  To return to boundary selection, click '< Back'.",
                                       "  \u2022  To quit GAT, click '", quitopt, "'."),
                         minvalue = min(data.frame(shp)[, agglist$var2], na.rm = TRUE))
            gats$msg = paste0("Please enter a valid minimum value for ",
                              agglist$var2, "\n", "that is at least ", gats$minvalue, ".")
            agglist$minval2 <- inputGATvalue(title = gats$title, help = gats$help,
                                             message = gats$msg,
                                             defaulttext = gats$minvalue,
                                             helppage = "inputGATvalue",
                                             step = step, backopt = backopt,
                                             bgcol = bgcol, quitopt = quitopt,
                                             buttoncol = buttoncol)
            if (is.null(agglist$minval2)) {
              x <- confirmGATquit()
              if (x == "quit") {
                agglist$var1 <- "cancel"
                agglist$minval2 <- 0
                error <- FALSE
              } else {
                agglist$minval2 <- "repeat"
              }
            }
            if (agglist$minval2 == "back") {
              agglist$var1 <- "repeat"
              agglist$minval2 <- 0
              error <- TRUE
            } else if (agglist$minval2 == "cancel") {
              agglist$var1 <- "cancel"
              agglist$minval2 <- 0
              error <- FALSE
            }
          }
          if (as.numeric(gsub(",", "", agglist$minval2)) <
              min(data.frame(shp)[, agglist$var2], na.rm = TRUE)) {
            msg <- paste("Your selected minimum value of", agglist$minvalue2,
                         "is lower than the minimum value of", agglist$var2, "(",
                         min(data.frame(shp)[, agglist$var2], na.rm = TRUE), "),",
                         "so no areas would be merged. Please change either your",
                         "aggregation variable or your desired minimum value.")
            tcltk::tkmessageBox(title = "Please change selections", message = msg,
                                type = "yesno", icon = "info")
            error <- TRUE
          }

        }


        # issues with maximum value 2 ####
        if (agglist$maxval2 != "none") {
          while (is.na(suppressWarnings(as.numeric(gsub(",", "", agglist$maxval2))))) {
            gats <- list(title = paste("Maximum value for", agglist$var2),
                         help = paste0("Enter a valid number that is greater than ",
                                       agglist$minval2, ". \n",
                                       "  \u2022  To continue,  click 'Next >'. \n",
                                       "  \u2022  To return to boundary selection, click '< Back'.",
                                       "  \u2022  To quit GAT, click '", quitopt, "'."),
                         maxvalue = sum(data.frame(shp)[, agglist$var2], na.rm = TRUE),
                         minvalue = min(data.frame(shp)[, agglist$var2], na.rm = TRUE))
            if (agglist$minval2 != "none") {
              gats$msg <- paste0("Please enter a valid maximum value for ",
                                 agglist$var2, "\n", "that is greater than ",
                                 agglist$minval2, ".")
            } else {
              gats$msg <- paste0("Please enter a valid maximum value for ",
                                 agglist$var2, "\n", "that is greater than ",
                                 gats$minvalue, ".")
            }
            agglist$maxval2 <- inputGATvalue(title = gats$title, help = gats$help,
                                             message = gats$msg,
                                             defaulttext = gats$maxvalue,
                                             helppage = "inputGATvalue",
                                             step = step, backopt = backopt,
                                             bgcol = bgcol, quitopt = quitopt,
                                             buttoncol = buttoncol)
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
              agglist$maxval2 <- 0
              error <- TRUE
            } else if (agglist$maxval2 == "cancel") {
              agglist$var1 <- "cancel"
              agglist$maxval2 <- 0
              error <- FALSE
            }
          }
          if (agglist$minval2 != "none") {
            if (suppressWarnings(as.numeric(gsub(",", "", agglist$maxval2))) <=
                suppressWarnings(as.numeric(gsub(",", "", agglist$minval2)))) {
              msg <- paste("Please select a maximum value that is larger than the",
                           "minimum value for your second aggregation variable.",
                           "If you do not want to define a maximum value, please",
                           "type 'none'.")
              tcltk::tkmessageBox(title = "Please re-check maximum and minimum values",
                                  message = msg, type = "ok", icon = "warning")
              error <- TRUE
            }
          }
        }

      }
    }
  }

  # define defaults - move this to after confirmation dialog?
  if (!agglist$var1 %in% c("cancel", "repeat", "back")) {
    # vector should include only relevant var and not ignored
    # if ignored, assign min or total of var
    if (agglist$maxval1 == "none") {
      agglist$maxval1 <- sum(data.frame(shp)[, agglist$var1], na.rm = TRUE)
    }
    if (agglist$var2 == "NONE") {
      agglist$maxval2 <- agglist$maxval1
      agglist$minval2 <- agglist$minval1
    } else {
      if (agglist$minval2 == "none") {
        agglist$minval2 = min(data.frame(shp)[, agglist$var2], na.rm = TRUE)
      }
      if (agglist$maxval2 == "none") {
        agglist$maxval2 = sum(data.frame(shp)[, agglist$var2], na.rm = TRUE)
      }
    }
  }
  return(agglist)
}
