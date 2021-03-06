#' Identify GAT Population Variable
#'
#' @description
#' This function opens a dialog window for the user to select which variable
#' in the population shapefile (if population-weighted centroids are requested)
#' should be used to determine area populations.
#'
#' \if{html}{\figure{identifyGATpopulation.png}{options: width="340px"
#'                   alt="Figure: Screenshot of dialog to select base population"}}
#'
#' Select your desired base population variable. Then click on one of the
#' following buttons.
#'
#' * Click \code{Next} to continue to the next step.
#' * Click \code{Cancel} to end GAT.
#' * Click \code{Back} to return to the previous step.
#' * Click \code{Help} to get further guidance and open this manual.
#'
#'
#'
#' @param varlist A vector of names of variables. Within GAT, these variables
#'                must be numeric, but the function does not require that.
#' @param step    Integer step in the GAT program, for help reference.
#' @param var     Population variable to use for weighting, if pre-selected.
#' @param backopt Boolean denoting whether to include the back button.
#'
#' @examples
#'
#' \donttest{
#' # identify variable to aggregate
#' identifyGATpopulation(
#'   varlist = c("Pop_tot", "F_tot", "M_tot"),
#'   step = 7
#' )
#' }
#'
#' @export

identifyGATpopulation <- function(varlist, step = 8, var = "NONE",
                                  backopt = TRUE) {
  noofchoices <- length(varlist)
  if (noofchoices == 1) {
    msg <- paste0("The only numeric variable is ", varlist,
                  ". It will be used as the base population.")
    x <- tcltk::tkmessageBox(title = "Base population variable", message = msg,
                             type = "yesno", icon = "info")
    if (tcltk::tclvalue(x) == "yes") {
      popvar <- varlist
    } else if (tcltk::tclvalue(x) == "no") {
      popvar <- "back"
    }
  } else if (noofchoices > 1) {
    title <- "Base Population Variable"
    msg <- "Calculate base population weights from"
      hlp <- paste0("Select your base population variable.  \n",
                    "  \u2022  To continue,  click 'Next >'. \n",
                    "  \u2022  To return to merge type selection, click '< Back'. \n",
                    "  \u2022  To quit GAT, click 'Cancel'.")

    popvar <- "repeat"

    while (popvar == "repeat") {
      popvar <- inputGATvariable(mylist = varlist, myvar = var,
                                 instruction = msg, valuebox = FALSE,
                                 title = title, help = hlp, step = step,
                                 helppage = "identifyGATpopulation",
                                 backopt = backopt)$myvar

      if (is.null(popvar)) {
        x <- confirmGATquit()
        if (x == "quit") {
          popvar <- "cancel"
        } else {
          popvar <- "repeat"
        }
      } else if (length(popvar) == 0) {
        msg <- paste("Please select a population variable.")
        popvar <- "repeat"
      }
    }
  }
  return(popvar)
}
