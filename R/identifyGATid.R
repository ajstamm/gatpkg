#' Identify GAT Shapefile Identifier
#'
#' @description
#' This function opens a dialog window for the user to select which
#' identifying variable in the shapefile should be used to label the polygons
#' for aggregation. The dialog window looks like this.
#'
#' \figure{identifyGATid.png}
#'
#' *Figure: Dialog to select your identifier variable*
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
#' @param backopt Boolean denoting whether to include the back button.
#'
#' @examples
#'
#' if (interactive()) {
#' # identify the character variable to use as the ID
#' identifyGATid(mapdata = hftown)
#' }
#'
#' @export

identifyGATid <- function(mapdata, step = 2, backopt = TRUE) {
  iditems <- checkGATvariabletypes(mapdata, type = "character")
  idlist <- c()
  for (i in 1:length(iditems)) {
    t <- table(data.frame(mapdata)[, iditems[i]])
    idlist[i] <- length(t) == nrow(mapdata)
  }

  iditems <- iditems[idlist == TRUE]
  noofchoices <- length(iditems)

  if (noofchoices == 1) {
    msg <- paste0("The only variable in the dataset that uniquely identifies \n",
                  "the areas is ", iditems, ".")
    hlp <- "Click 'Yes' to continue or '< Back' to return to shapefile selection."

    mycancel <- inputGATmessage(title = "Identification Variable",
                                help = hlp, step = step, msg = msg,
                                helptitle = "inputGATmessage",
                                helppage = "inputGATmessage",
                                buttonopt = "Cancel",
                                backopt = backopt)

    if (is.null(mycancel)) {
      myidvar <- as.character(iditems)
    } else if (mycancel == "Yes") {
      myidvar <- as.character(iditems)
    } else {
      myidvar <- mycancel
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
                                    helppage = "identifyGATid",
                                    backopt = backopt)
      if (!is.null(myoptions)) {
        if (length(myoptions$myvar) > 0) {
          myidvar <- myoptions$myvar
        } else {
          msg <- "Please select a variable to identify the areas."
          myidvar <- "repeat"
        }
      } else {
        x <- confirmGATquit()
        myidvar <- if (x == "quit") "cancel" else "repeat"
      }
    }
  } else if (noofchoices < 1) myidvar <- "missing"
  return(myidvar)
}
