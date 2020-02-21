#' Identify GAT Boundary
#'
#' @description
#' This function opens a dialog window for the user to select a character
#' variable to prioritize larger geographic areas such as counties in
#' which to aggregate. The dialog window looks like this.
#'
#' \if{html}{\figure{identifyGATboundary.png}{options: width="340px"
#'                   alt="Figure: Screenshot of dialog to select your boundary"}}
#'
#' Select your desired boundary variable. If you do not want to include a
#' boundary, select "NONE". If you want to enforce the boundary, check the box
#' on the right as shown. Then click on one of the following buttons.
#'
#' * Click \code{Next} to continue to the next step.
#' * Click \code{Cancel} to end GAT.
#' * Click \code{Back} to return to the previous step.
#' * Click \code{Help} to get further guidance and open this manual.
#'
#' @details
#' This function reads in a dataset and extracts the names of variables that
#' are (1) character and (2) not unique, since only these variables will be
#' suitable for defining larger geographic areas in GAT.
#'
#' @param data     A data frame, intended to be read from a shapefile DBF.
#' @param step     Integer step in the GAT program, for help reference.
#' @param boundary Boundary variable, if pre-selected. Defaults to "NONE",
#'                 which means no boundary selected.
#' @param borders  Boolean denoting whether to enforce boundary. Defaults
#'                 to FALSE.
#' @param myvar    The boundary variable, if pre-selected.
#' @param check    The checkbox setting, if pre-selected. Currently does
#'                 not do anything.
#' @param backopt  Boolean denoting whether to include the back button.
#'
#' @examples
#'
#' \donttest{
#' # select boundary variable
#' identifyGATboundary(
#'   data = hftown@data,
#' )
#' }
#'
#' @export

identifyGATboundary <- function(data, step = 3, boundary = "NONE",
                                borders = FALSE,
                                myvar = "NONE", check = FALSE,
                                backopt = TRUE) {
  # should be at least two choices, because we add "NONE"
  charlistitems <- checkGATvariabletypes(data, type = "character")
  idlist <- c()
  for (i in 1:length(charlistitems)) {
    t <- table(data[, charlistitems[i]])
    idlist[i] <- length(t) == nrow(data)
  }

  boundaryitems <- c("NONE", charlistitems[idlist == FALSE])

  noofchoices <- length(boundaryitems)

  if (noofchoices > 1) {
    msg <- paste("Please select the variable that \nidentifies boundaries within",
                 "\nwhich GAT should merge.")
    chk <- paste("Click here to enforce \nboundaries, even if the \narea",
                 "total will be less \nthan the desired \nminimum.")
    hlp <- paste0("Select your boundary variable. If you do not want a \n",
                  "boundary variable, select 'NONE'. \n",
                  "  \u2022  To continue,  click 'Next >'. \n",
                  "  \u2022  To return to identifier selection, click '< Back'. \n",
                  "  \u2022  To quit GAT, click 'Cancel'.")

    error <- TRUE

    while (error) {
      boundaryvars <- inputGATvariable(mylist = boundaryitems, instruction = msg,
                                     title = "Boundary Variable", checkopt = chk,
                                     checkbox = TRUE, help = hlp, step = step,
                                     helppage = "identifyGATboundary",
                                     myvar = boundary, check = borders,
                                     backopt = backopt)
      error <- FALSE
      if (is.null(boundaryvars)) {
        x <- confirmGATquit()
        if (x == "quit") {
          boundaryvars <- list(myvar = "cancel", check = FALSE)
        } else {
          error < TRUE
        }
      } else if (length(boundaryvars$myvar) == 0) {
        msg <- paste("Please select a variable to identify \nyour boundaries,",
                     "or select 'NONE' \nif you do not want a boundary.")
        error <- TRUE
      }
    }
  } else {
    boundaryvars <- list(myvar = "NONE", check = FALSE)
    msg <- paste("The shapefile does not contain a suitable boundary variable.")
    tcltk::tkmessageBox(title = "No suitable boundary variable", message = msg,
                        type = "ok", icon = "warning")
  }
  return(boundaryvars)
}
