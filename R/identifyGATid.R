#' Identify GAT Shapefile Identifier
#'
#' @description
#' This function opens a dialog window for the user to select which
#' identifying variable in the shapefile should be used to label the polygons
#' for aggregation. The dialog window looks like this.
#'
#' \if{html}{\figure{identify_id.png}{options: width="340px"
#'                   alt="Figure: Screenshot of dialog to select your identifier"}}
#'
#' Click on your desired identifier in the drop-down list. Then click on one
#' of the following buttons.
#'
#' * Click \code{Next} to continue to the next step.
#' * Click \code{Cancel} to end GAT.
#' * Click \code{Back} to return to the previous step.
#' * Click \code{Help} to get further guidance and open this manual.
#'
#' @details
#' This variable will be duplicated and modified by the aggregation to provide
#' new identifiers for newly created polygons. Any unmerged polygons will
#' retain their original identifiers.
#'
#' @param mapdata A data frame, intended to be read from a shapefile DBF.
#' @param step    Integer step in the GAT program, for help reference.
#'
#' @examples
#'
#' \donttest{
#' # identify the character variable to use as the ID
#' identifyGATid(
#'   mapdata = hftown@data
#' )
#' }
#'
#' @export

identifyGATid <- function(mapdata, step = 2) {
  iditems <- checkGATvariabletypes(mapdata, type = "character")
  idlist <- c()
  for (i in 1:length(iditems)) {
    t <- table(mapdata[, iditems[i]])
    idlist[i] <- length(t) == nrow(mapdata)
  }

  iditems <- iditems[idlist == TRUE]
  noofchoices <- length(iditems)

  if (noofchoices == 1) {
    msg <- paste("The only variable in the dataset that uniquely",
                 "identifies the areas is", iditems)
    mycancel <- tcltk::tkmessageBox(message = msg, type = "okcancel",
                                    title = "Identification Variable")
    x <- tcltk::tclvalue(mycancel)

    if (x == "ok") {
      myidvar <- iditems
    } else {
      myidvar <- "back"
    }
  # need to use previous myidvar as default if available
  } else if (noofchoices > 1) {
    hlp <- paste0("Select your identifying variable. \n",
                  "  \u2022  To continue,  click 'Next >'. \n",
                  "  \u2022  To return to shapefile selection, click '< Back'. \n",
                  "  \u2022  To quit GAT, click 'Cancel'.")
    msg <- "Select a variable that uniquely identifies the areas:"

    myidvar <- "repeat"

    while (myidvar == "repeat") {
      myoptions <- inputGATvariable(mylist = iditems, instruction = msg,
                                    title = "Identification Variable",
                                    step = step, help = hlp,
                                    helptitle = "the identification variable",
                                    helppage = "identifyGATid")
      if (!is.null(myoptions)) {
        if (length(myoptions$myvar) > 0) {
          myidvar <- myoptions$myvar
        } else {
          msg <- "Please select a variable to identify the areas."
          myidvar <- "repeat"
        }
      } else {
        x <- confirmGATquit()
        if (x == "quit") {
          myidvar <- "cancel"
        } else {
          myidvar <- "repeat"
        }
      }
    }

  } else if (noofchoices < 1) {
    myidvar <- "missing"
  }
  return(myidvar)
}
